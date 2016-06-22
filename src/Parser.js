/* eslint no-constant-condition:0 */
var functions = require("./functions");
var environments = require("./environments");
var Lexer = require("./Lexer");
var symbols = require("./symbols");
var utils = require("./utils");

var parseData = require("./parseData");
var ParseError = require("./ParseError");

/**
 * This file contains the parser used to parse out a TeX expression from the
 * input. Since TeX isn't context-free, standard parsers don't work particularly
 * well.
 *
 * The strategy of this parser is as such:
 *
 * The main functions (the `.parse...` ones) take a position in the current
 * parse string to parse tokens from. The lexer (found in Lexer.js, stored at
 * this.lexer) also supports pulling out tokens at arbitrary places. When
 * individual tokens are needed at a position, the lexer is called to pull out a
 * token, which is then used.
 *
 * The parser has a property called "mode" indicating the mode that
 * the parser is currently in. Currently it has to be one of "math" or
 * "text", which denotes whether the current environment is a math-y
 * one or a text-y one (e.g. inside \text). Currently, this serves to
 * limit the functions which can be used in text mode.
 *
 * The main functions then return an object which contains the useful data that
 * was parsed at its given point, and a new position at the end of the parsed
 * data. The main functions can call each other and continue the parsing by
 * using the returned position as a new starting point.
 *
 * There are also extra `.handle...` functions, which pull out some reused
 * functionality into self-contained functions.
 *
 * The earlier functions return ParseNodes.
 * The later functions (which are called deeper in the parse) sometimes return
 * ParseFuncOrArgument, which contain a ParseNode as well as some data about
 * whether the parsed object is a function which is missing some arguments, or a
 * standalone object which can be used as an argument to another function.
 */

/**
 * Main Parser class
 */
function Parser(input, settings) {
    // Make a new lexer
    this.lexer = new Lexer(input);
    // Store the settings for use in parsing
    this.settings = settings;
}

var ParseNode = parseData.ParseNode;

/**
 * Checks a result to make sure it has the right type, and throws an
 * appropriate error otherwise.
 *
 * @param {boolean=} consume whether to consume the expected token,
 *                           defaults to true
 */
Parser.prototype.expect = function(text, consume) {
    if (this.nextToken.text !== text) {
        throw new ParseError(
            "Expected '" + text + "', got '" + this.nextToken.text + "'",
            this.lexer, this.nextToken.position
        );
    }
    if (consume !== false) {
        this.consume();
    }
};

/**
 * Considers the current look ahead token as consumed,
 * and fetches the one after that as the new look ahead.
 */
Parser.prototype.consume = function() {
    this.pos = this.nextToken.position;
    this.nextToken = this.lexer.lex(this.pos, this.mode);
};

/**
 * Main parsing function, which parses an entire input.
 *
 * @return {?Array.<ParseNode>}
 */
Parser.prototype.parse = function() {
    // Initialize the lexer
    this.mode = "math";
    this.pos = 0;
    this.nextToken = this.lexer.lex(this.pos, this.mode);

    // Begin parsing expression
    var parse = this.parseExpression();
    this.expect("EOF", false);
    return parse;
};


// These flags are used for the parser to keep track of whether it is
// currently parsing a styles expression or an infix expression. This
// is needed to handle inline styles in our recursive strategy properly.
var STYLES = 1;
var INFIX = 2;

var endOfExpression = ["}", "\\end", "\\right", "&", "\\\\", "\\cr", "EOF"];

/**
 * Parses an "expression", which is a list of atoms.
 *
 * @param {number}  breakOnInfix Should the parsing stop when we hit infix
 *                  nodes? This happens when functions have higher precendence
 *                  than infix nodes in implicit parses.
 *
 * @param {?string} breakOnToken The token that the expression should end with,
 *                  or `null` if something else should end the expression.
 *
 * @return {ParseNode[]}
 */
Parser.prototype.parseExpression = function(breakOnInfix, breakOnToken) {
    var body = [];
    // Keep adding atoms to the body until we can't parse any more atoms (either
    // we reached the end, a }, or a \right)
    while (true) {
        var lex = this.nextToken;
        if (endOfExpression.indexOf(lex.text) !== -1) {
            break;
        }
        if (breakOnToken && lex.text === breakOnToken) {
            break;
        }

        // Before we parse anything, we handle infix operators
        if (lex.text === "\\over" || lex.text === "\\choose") {
            // If we are already in an infix, we throw an error.
            if (breakOnInfix === INFIX) {
                throw new ParseError("Only one infix operator per group",  
                    this.lexer, -1);                
            } else if (breakOnInfix === STYLES) {
                break;
            }

            this.consume();
            // Infix operators are replaced by their respective LaTeX functions.
            var func  = (lex.text === "\\over") ? "\\frac" : "\\binom";
            var denom = this.parseExpression(INFIX, breakOnToken);
            return body = [ this.compileInfix(func, body, denom) ];
        }

        var atom = this.parseAtom();

        if (!atom) {
            if (!this.settings.throwOnError) {
                var errorNode = this.handleUnsupportedCmd();
                body.push(errorNode);
                continue;
            }
            throw new ParseError(
                'Unsupported command "' + lex.text + '"', this.lex, this.pos);
        }

        body.push(atom);
    }
    return body;
};

/**
 * Rewrites infix operators such as \over with corresponding commands such
 * as \frac.
 *
 * @returns {Array}
 */
Parser.prototype.compileInfix = function(funcName, numerBody, denomBody) {
    var numerNode, denomNode;
    if (numerBody.length === 1 && numerBody[0].type === "ordgroup") {
        numerNode = numerBody[0];
    } else {
        numerNode = new ParseNode("ordgroup", numerBody, this.mode);
    }

    if (denomBody.length === 1 && denomBody[0].type === "ordgroup") {
        denomNode = denomBody[0];
    } else {
        denomNode = new ParseNode("ordgroup", denomBody, this.mode);
    }

    var value = this.callFunction(
        funcName, [numerNode, denomNode], null);
    return new ParseNode(value.type, value, this.mode);
};

// The greediness of a superscript or subscript
var SUPSUB_GREEDINESS = 1;

/**
 * Attempt to expand a function as an argument to another function.
 * This will only work if the argument function is strictly greedier
 * than the parent function. Otherwise we throw an error.
 * 
 * @param {number} parentGreediness The greediness of the parent function.
 * @returns {?ParseNode}
 */

Parser.prototype.tryFunctionExpand = function(parentGreediness) {
    var token = this.nextToken.text;

    if (this.isFunction(token)) {
        var funcGreediness = functions[token].greediness;
        if (funcGreediness > parentGreediness) {
            return this.parseFunction();
        } else {
            throw new ParseError(
                "Got function '" + token + "' with no arguments.",
                this.lexer, this.pos);
        }
    } else {
        return null;
    }
};

/**
 * Handle a subscript or superscript with nice errors.
 */
Parser.prototype.handleSupSubscript = function(name) {
    var symPos = this.pos;
    this.consume();

    var group = this.parseSymbol() ||
                this.parseGroup()  ||
                this.tryFunctionExpand(SUPSUB_GREEDINESS);

    return group ? group : this.expectedGroupError(name, symPos + 1);
};

/**
 * Converts the textual input of an unsupported command into a text node
 * contained within a color node whose color is determined by errorColor
 */
Parser.prototype.handleUnsupportedCmd = function() {
    var text = this.nextToken.text;
    var textordArray = [];

    for (var i = 0; i < text.length; i++) {
        textordArray.push(new ParseNode("textord", text[i], "text"));
    }

    var textNode = new ParseNode(
        "text",
        {
            body: textordArray,
            type: "text",
        },
        this.mode);

    var colorNode = new ParseNode(
        "color",
        {
            color: this.settings.errorColor,
            value: [textNode],
            type: "color",
        },
        this.mode);

    this.consume();
    return colorNode;
};

/**
 * Parses a group with optional super/subscripts.
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseAtom = function() {
    // First we check if we have a either a Symbol, a
    // function, an implicit group, or explicit group, etc.
    var base = this.parseSymbol()        ||
               this.parseImplicitGroup() ||
               this.parseGroup()         ||
               this.parseFunction();

    // In text mode, we don't have superscripts or subscripts
    if (this.mode === "text") {
        return base;
    } 

    // Note that base may be empty (i.e. null) at this point.
    // We now parse for a few non-standard command sequences
    // like \limits, and handle a sub/superscripts commands.
    var superscript;
    var subscript;
    while (true) {
        // Lex the first token
        var lex = this.nextToken;

        if (lex.text === "\\limits" || lex.text === "\\nolimits") {
            // We got a limit control
            if (!base || base.type !== "op") {
                throw new ParseError(
                    "Limit controls must follow a math operator",
                    this.lexer, this.pos);
            } else {
                var limits = lex.text === "\\limits";
                base.value.limits = limits;
                base.value.alwaysHandleSupSub = true;
            }
            this.consume();
        } else if (lex.text === "^") {
            // We got a superscript start
            if (superscript) {
                throw new ParseError(
                    "Double superscript", this.lexer, this.pos);
            }
            superscript = this.handleSupSubscript("superscript");
        } else if (lex.text === "_") {
            // We got a subscript start
            if (subscript) {
                throw new ParseError(
                    "Double subscript", this.lexer, this.pos);
            }
            subscript = this.handleSupSubscript("subscript");
        } else if (lex.text === "'") {
            // We got a prime
            var prime = new ParseNode("textord", "\\prime", this.mode);

            // Many primes can be grouped together, so we handle this here
            var primes = [prime];
            this.consume();
            // Keep lexing tokens until we get something that's not a prime
            while (this.nextToken.text === "'") {
                // For each one, add another prime to the list
                primes.push(prime);
                this.consume();
            }
            // Put them into an ordgroup as the superscript
            superscript = new ParseNode("ordgroup", primes, this.mode);
        } else {
            // If it wasn't ^, _, or ', stop parsing super/subscripts
            break;
        }
    }

    if (superscript || subscript) {
        // If we got either a superscript or subscript, create a supsub
        return new ParseNode("supsub", {
            base: base,
            sup: superscript,
            sub: subscript,
        }, this.mode);
    } else {
        // Otherwise return the original body
        return base;
    }
};

// A list of the size-changing functions, for use in parseImplicitGroup
var sizeFuncs = [
    "\\tiny", "\\scriptsize", "\\footnotesize", "\\small", "\\normalsize",
    "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge",
];

// A list of the style-changing functions, for use in parseImplicitGroup
var styleFuncs = [
    "\\displaystyle", "\\textstyle", "\\scriptstyle", "\\scriptscriptstyle",
];

/**
 * Parses an implicit group, which is a group that starts at the end of a
 * specified, and ends right before a higher explicit group ends, or at EOL. It
 * is used for functions that appear to affect the current style, like \Large or
 * \textrm, where instead of keeping a style we just pretend that there is an
 * implicit grouping after it until the end of the group. E.g.
 *   small text {\Large large text} small text again
 * It is also used for \left and \right to get the correct grouping.
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseImplicitGroup = function() {
    var func = this.nextToken.text;
    var body;

    if (func === "\\left") {
        // Parse \left functino including the delimiter.
        var left = this.parseFunction();
        // Parse out the implicit body
        body = this.parseExpression();
        // Check the next token
        this.expect("\\right", false);
        var right = this.parseFunction();
        return new ParseNode("leftright", {
            body: body,
            left: left.value.value,
            right: right.value.value,
        }, this.mode);
    } else if (func === "\\begin") {
        // begin...end is similar to left...right
        var begin = this.parseFunction();
        var envName = begin.value.name;
        if (!environments.hasOwnProperty(envName)) {
            throw new ParseError(
                "No such environment: " + envName,
                this.lexer, begin.value.namepos);
        }
        // Build the environment object. Arguments and other information will
        // be made available to the begin and end methods using properties.
        var env = environments[envName];
        var args = this.parseArguments("\\begin{" + envName + "}", env);
        var context = {
            mode: this.mode,
            envName: envName,
            parser: this,
            lexer: this.lexer,
            positions: args.pop(),
        };
        var result = env.handler(context, args);
        this.expect("\\end", false);
        var end = this.parseFunction();
        if (end.value.name !== envName) {
            throw new ParseError(
                "Mismatch: \\begin{" + envName + "} matched " +
                "by \\end{" + end.value.name + "}",
                this.lexer /* , end.value.namepos */);
            // TODO: Add position to the above line and adjust test case,
            // requires #385 to get merged first
        }
        result.position = end.position;
        return result;
    } else if (utils.contains(sizeFuncs, func)) {
        // If we see a sizing function, parse out the implict body
        this.consume();
        body = this.parseExpression();
        return new ParseNode("sizing", {
            // Figure out what size to use based on the list of functions above
            size: "size" + (utils.indexOf(sizeFuncs, func) + 1),
            value: body,
        }, this.mode);
    } else if (utils.contains(styleFuncs, func)) {
        // If we see a styling function, parse out the implict body
        this.consume();
        body = this.parseExpression(STYLES);
        return new ParseNode("styling", {
            // Figure out what style to use by pulling out the style from
            // the function name
            style: func.slice(1, func.length - 5),
            value: body,
        }, this.mode);
    } else {
        return null;
    }
};

/**
 * Check if we have a function, and if so, parse that function along
 * with its required arguments.  Otherwise do nothing.
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseFunction = function() {
    var func = this.nextToken.text;
    var funcData = functions[func];

    if (funcData) {
        if (this.mode === "text" && !funcData.allowedInText) {
            throw new ParseError(
                "Can't use function '" + func + "' in text mode",
                this.lexer, this.position);
        }

        this.consume();
        var args = this.parseArguments(func, funcData);
        var result = this.callFunction(func, args, args.pop());
        return new ParseNode(result.type, result, this.mode);
    } else {
        return null;
    }
};

/**
 * Call a function handler with a suitable context and arguments.
 */
Parser.prototype.callFunction = function(name, args, positions) {
    var context = {
        funcName: name,
        parser: this,
        lexer: this.lexer,
        positions: positions,
    };
    return functions[name].handler(context, args);
};

/**
 * Parses the arguments of a function or environment
 *
 * @param {string} func  "\name" or "\begin{name}"
 * @param {{numArgs:number,numOptionalArgs:number|undefined}} funcData
 * @return the array of arguments, with the list of positions as last element
 */
Parser.prototype.parseArguments = function(func, funcData) {
    var numArgs = funcData.numArgs;
    var numOptArgs = funcData.numOptionalArgs;

    if (numArgs === 0 && numOptArgs === 0) {
        return [[this.pos]];
    }

    var baseGreediness = funcData.greediness;
    var positions = [this.pos];
    var args = [];

    for (var i = 0; i < numOptArgs + numArgs; i++) {
        var argType = funcData.argTypes && funcData.argTypes[i];
        var arg;

        if (i < numOptArgs) {
            if (argType) {
                arg = this.parseSpecialGroup(argType, true);
            } else {
                arg = this.parseOptionalGroup(argType);
            }
        } else {
            if (argType) {
                arg = this.parseSpecialGroup(argType, false, baseGreediness);
            } else {
                arg = this.parseSymbol() ||
                      this.parseGroup()  ||
                      this.tryFunctionExpand(baseGreediness);
            }

            // Error if no argument is given for required argument
            if (!arg) {
                arg = this.expectedGroupError(func, this.pos);
            }
        }

        args.push(arg);
        positions.push(this.pos);
    }

    args.push(positions);
    return args;
};


/**
 * Parses a group when the mode is changing. Takes a position, a new mode, and
 * an outer mode that is used to parse the outside.
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseSpecialGroup = function(innerMode, optional, parentGreediness) {
    var outerMode = this.mode;
    // Handle `original` argTypes
    if (innerMode === "original") {
        innerMode = outerMode;
    }

    if (innerMode === "color" || innerMode === "size") {
        // color and size modes are special because they should have braces and
        // should only lex a single symbol inside
        var openBrace = this.nextToken;
        if (optional && openBrace.text !== "[") {
            // optional arguments should return null if they don't exist
            return null;
        }
        // The call to expect will lex the token after the '{' in inner mode
        this.mode = innerMode;
        this.expect(optional ? "[" : "{");
        var inner = this.nextToken;
        this.mode = outerMode;
        var data;
        if (innerMode === "color") {
            data = inner.text;
        } else {
            data = inner.data;
        }
        this.consume(); // consume the token stored in inner
        this.expect(optional ? "]" : "}");
        return new ParseNode(innerMode, data, outerMode);
    } else if (innerMode === "text") {
        // text mode is special because it should ignore the whitespace before
        // it
        var whitespace = this.lexer.lex(this.pos, "whitespace");
        this.pos = whitespace.position;
    }

    // By the time we get here, innerMode is one of "text" or "math".
    // We switch the mode of the parser, recurse, then restore the old mode.
    this.mode = innerMode;
    this.nextToken = this.lexer.lex(this.pos, innerMode);
    var res;
    if (optional) {
        res = this.parseOptionalGroup();
    } else {
        res = this.parseSymbol() || this.parseGroup() 
           || this.tryFunctionExpand(parentGreediness);
    }
    this.mode = outerMode;
    this.nextToken = this.lexer.lex(this.pos, outerMode);
    return res;
};

/**
 * Parses a group, which is either a single nucleus (like "x") or an expression
 * in braces (like "{x+y}")
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseGroup = function() {
    // Try to parse an open brace
    if (this.nextToken.text === "{") {
        // If we get a brace, parse an expression
        this.consume();
        var expression = this.parseExpression();
        // Make sure we get a close brace
        this.expect("}");
        return new ParseNode("ordgroup", expression, this.mode);
    } else {
        return null;
    }
};

/**
 * Parses a group, which is an expression in brackets (like "[x+y]").
 * This is used for optional arguments like in \sqrt[3]{2}
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseOptionalGroup = function() {
    // Try to parse an open bracket
    if (this.nextToken.text === "[") {
        // If we get a brace, parse an expression
        this.consume();
        var expression = this.parseExpression(false, "]");
        // Make sure we get a close bracket
        this.expect("]");
        return new ParseNode("ordgroup", expression, this.mode);
    } else {
        return null;
    }
};

/**
 * Parse a single symbol out of the string.  This is anything that
 * can be found in symbols.js, except for accents which behave more
 * like functions than symbols.
 *
 * @return {?ParseNode}
 */
Parser.prototype.parseSymbol = function() {
    // TODO (cbreeden) hopefully we can get rid of the function check soon.
    var token = this.nextToken;
    if (this.isFunction(token.text)) {
        return null;
    }

    var symbol = symbols[this.mode][token.text];
    if (symbol) {
        this.consume();
        return new ParseNode(symbol.group, token.text, this.mode);
    } else {
        return null;
    }
};

Parser.prototype.isFunction = function(name) {
    return functions.hasOwnProperty(name);
}

/***
 * 
 */

Parser.prototype.expectedGroupError = function(symbol, pos) {
    if (!this.settings.throwOnError && this.nextToken.text[0] === "\\") {
        return this.handleUnsupportedCmd();
    } else {
        throw new ParseError(
            "Expected group after '" + symbol + "'",
            this.lexer,
            pos
        );
    }
};

Parser.prototype.ParseNode = ParseNode;

module.exports = Parser;
