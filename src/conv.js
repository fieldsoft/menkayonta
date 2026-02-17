(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (Object.prototype.hasOwnProperty.call(value, key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	var unwrapped = _Json_unwrap(value);
	if (!(key === 'toJSON' && typeof unwrapped === 'function'))
	{
		object[key] = unwrapped;
	}
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $author$project$Converter$OriginalS = {$: 'OriginalS'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$parser$Parser$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var $elm$core$String$length = _String_length;
var $elm$parser$Parser$Advanced$end = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				$elm$core$String$length(s.src),
				s.offset) ? A3($elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0.a;
		var parseB = _v1.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v2 = parseA(s0);
				if (_v2.$ === 'Bad') {
					var p = _v2.a;
					var x = _v2.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v2.a;
					var a = _v2.b;
					var s1 = _v2.c;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3(
							$elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $elm$parser$Parser$keeper = $elm$parser$Parser$Advanced$keeper;
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0.a;
			var _v1 = parse(s0);
			if (_v1.$ === 'Good') {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (step.$ === 'Loop') {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
			});
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (step.$ === 'Loop') {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (_v1.$ === 'Good') {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parseA(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					var _v2 = callback(a);
					var parseB = _v2.a;
					var _v3 = parseB(s1);
					if (_v3.$ === 'Bad') {
						var p2 = _v3.a;
						var x = _v3.b;
						return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _v3.a;
						var b = _v3.b;
						var s2 = _v3.c;
						return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$core$String$slice = _String_slice;
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0.a;
		return $elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _v1 = parse(s0);
				if (_v1.$ === 'Bad') {
					var p = _v1.a;
					var x = _v1.b;
					return A2($elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _v1.a;
					var a = _v1.b;
					var s1 = _v1.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3($elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3($elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $bellroy$elm_email$Email$parseDomain = A2(
	$elm$parser$Parser$andThen,
	function (a) {
		return ($elm$core$String$length(a) < 1) ? $elm$parser$Parser$problem('Domain has to be at least 1 character long.') : $elm$parser$Parser$succeed(a);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompWhile(
				function (a) {
					return ($elm$core$Char$isAlphaNum(a) || _Utils_eq(
						a,
						_Utils_chr('-'))) && ((!_Utils_eq(
						a,
						_Utils_chr('@'))) && (!_Utils_eq(
						a,
						_Utils_chr('.'))));
				}))));
var $elm$core$String$contains = _String_contains;
var $elm$core$String$endsWith = _String_endsWith;
var $elm$core$String$startsWith = _String_startsWith;
var $elm$core$String$trim = _String_trim;
var $bellroy$elm_email$Email$parseLocalPart = A2(
	$elm$parser$Parser$andThen,
	function (localPart) {
		return (A2($elm$core$String$startsWith, '.', localPart) || (A2($elm$core$String$endsWith, '.', localPart) || A2($elm$core$String$contains, '..', localPart))) ? $elm$parser$Parser$problem('Local part can\'t start or end with a dot, nor can there be double dots.') : ((!_Utils_eq(
			$elm$core$String$trim(localPart),
			localPart)) ? $elm$parser$Parser$problem('Local part can\'t be wrapped with whitespace.') : $elm$parser$Parser$succeed(localPart));
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompWhile(
				function (a) {
					return (!_Utils_eq(
						a,
						_Utils_chr('+'))) && ((!_Utils_eq(
						a,
						_Utils_chr('@'))) && ((!_Utils_eq(
						a,
						_Utils_chr('\\'))) && (!_Utils_eq(
						a,
						_Utils_chr('\"')))));
				}))));
var $elm$core$Basics$ge = _Utils_ge;
var $bellroy$elm_email$Email$parseTld = A2(
	$elm$parser$Parser$andThen,
	function (a) {
		return ($elm$core$String$length(a) >= 2) ? $elm$parser$Parser$succeed(a) : $elm$parser$Parser$problem('TLD needs to be at least 2 character long.');
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(_Utils_Tuple0),
			$elm$parser$Parser$chompWhile(
				function (a) {
					return $elm$core$Char$isUpper(a) || ($elm$core$Char$isLower(a) || _Utils_eq(
						a,
						_Utils_chr('-')));
				}))));
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 'ExpectingSymbol', a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$core$Basics$not = _Basics_not;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return $elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _v1.a;
			var newRow = _v1.b;
			var newCol = _v1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				$elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $bellroy$elm_email$Email$parseEmail = function () {
	var split = F2(
		function (_char, parser) {
			return A2(
				$elm$parser$Parser$loop,
				_List_Nil,
				function (r) {
					return $elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed(
										function (tld) {
											return $elm$parser$Parser$Loop(
												A2($elm$core$List$cons, tld, r));
										}),
									$elm$parser$Parser$symbol(
										$elm$core$String$fromChar(_char))),
								parser),
								A2(
								$elm$parser$Parser$map,
								function (_v1) {
									return $elm$parser$Parser$Done(
										$elm$core$List$reverse(r));
								},
								$elm$parser$Parser$succeed(_Utils_Tuple0))
							]));
				});
		});
	return A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				A2(
					$elm$parser$Parser$keeper,
					$elm$parser$Parser$succeed(
						F4(
							function (localPart, tags, domain, tlds) {
								var fullLocalPart = A2(
									$elm$core$String$join,
									'',
									_List_fromArray(
										[
											localPart,
											function () {
											if (!tags.b) {
												return '';
											} else {
												return '+' + A2($elm$core$String$join, '+', tags);
											}
										}()
										]));
								return ($elm$core$String$length(fullLocalPart) > 64) ? $elm$core$Maybe$Nothing : (($elm$core$List$length(tlds) < 1) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
									{domain: domain, localPart: localPart, tags: tags, tld: tlds}));
							})),
					$bellroy$elm_email$Email$parseLocalPart),
				A2(
					$elm$parser$Parser$ignorer,
					A2(
						split,
						_Utils_chr('+'),
						$bellroy$elm_email$Email$parseLocalPart),
					$elm$parser$Parser$symbol('@'))),
			$bellroy$elm_email$Email$parseDomain),
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				split,
				_Utils_chr('.'),
				$bellroy$elm_email$Email$parseTld),
			$elm$parser$Parser$end));
}();
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0.a;
		var _v1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_v1.$ === 'Good') {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (_v0.$ === 'Ok') {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $bellroy$elm_email$Email$fromString = function (string) {
	var _v0 = A2($elm$parser$Parser$run, $bellroy$elm_email$Email$parseEmail, string);
	if (_v0.$ === 'Ok') {
		var result = _v0.a;
		return result;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $TSFoster$elm_uuid$UUID$WrongFormat = {$: 'WrongFormat'};
var $TSFoster$elm_uuid$UUID$WrongLength = {$: 'WrongLength'};
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $TSFoster$elm_uuid$UUID$IsNil = {$: 'IsNil'};
var $TSFoster$elm_uuid$UUID$NoVersion = {$: 'NoVersion'};
var $TSFoster$elm_uuid$UUID$UUID = F4(
	function (a, b, c, d) {
		return {$: 'UUID', a: a, b: b, c: c, d: d};
	});
var $TSFoster$elm_uuid$UUID$UnsupportedVariant = {$: 'UnsupportedVariant'};
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $TSFoster$elm_uuid$UUID$isVariant1 = function (_v0) {
	var c = _v0.c;
	return (c >>> 30) === 2;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $TSFoster$elm_uuid$UUID$version = function (_v0) {
	var b = _v0.b;
	return 15 & (b >>> 12);
};
var $TSFoster$elm_uuid$UUID$fromInt32s = F4(
	function (a, b, c, d) {
		var wouldBeUUID = A4($TSFoster$elm_uuid$UUID$UUID, a, b, c, d);
		return ((!a) && ((!b) && ((!c) && (!d)))) ? $elm$core$Result$Err($TSFoster$elm_uuid$UUID$IsNil) : (($TSFoster$elm_uuid$UUID$version(wouldBeUUID) > 5) ? $elm$core$Result$Err($TSFoster$elm_uuid$UUID$NoVersion) : ((!$TSFoster$elm_uuid$UUID$isVariant1(wouldBeUUID)) ? $elm$core$Result$Err($TSFoster$elm_uuid$UUID$UnsupportedVariant) : $elm$core$Result$Ok(wouldBeUUID)));
	});
var $TSFoster$elm_uuid$UUID$forceUnsigned = $elm$core$Bitwise$shiftRightZfBy(0);
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $TSFoster$elm_uuid$UUID$nibbleValuesToU32 = F8(
	function (a, b, c, d, e, f, g, h) {
		return $TSFoster$elm_uuid$UUID$forceUnsigned((a << 28) | ((b << 24) | ((c << 20) | ((d << 16) | ((e << 12) | ((f << 8) | ((g << 4) | h)))))));
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $elm$core$String$toLower = _String_toLower;
var $TSFoster$elm_uuid$UUID$toNibbleValue = function (_char) {
	switch (_char.valueOf()) {
		case '0':
			return $elm$core$Maybe$Just(0);
		case '1':
			return $elm$core$Maybe$Just(1);
		case '2':
			return $elm$core$Maybe$Just(2);
		case '3':
			return $elm$core$Maybe$Just(3);
		case '4':
			return $elm$core$Maybe$Just(4);
		case '5':
			return $elm$core$Maybe$Just(5);
		case '6':
			return $elm$core$Maybe$Just(6);
		case '7':
			return $elm$core$Maybe$Just(7);
		case '8':
			return $elm$core$Maybe$Just(8);
		case '9':
			return $elm$core$Maybe$Just(9);
		case 'a':
			return $elm$core$Maybe$Just(10);
		case 'b':
			return $elm$core$Maybe$Just(11);
		case 'c':
			return $elm$core$Maybe$Just(12);
		case 'd':
			return $elm$core$Maybe$Just(13);
		case 'e':
			return $elm$core$Maybe$Just(14);
		case 'f':
			return $elm$core$Maybe$Just(15);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $TSFoster$elm_uuid$UUID$fromString = function (string) {
	var normalized = function (str) {
		return A2($elm$core$String$startsWith, 'urn:uuid:', str) ? A2($elm$core$String$dropLeft, 9, str) : ((A2($elm$core$String$startsWith, '{', str) && A2($elm$core$String$endsWith, '}', str)) ? A3($elm$core$String$slice, 1, -1, str) : str);
	}(
		$elm$core$String$toLower(
			A3(
				$elm$core$String$replace,
				'-',
				'',
				A3(
					$elm$core$String$replace,
					' ',
					'',
					A3(
						$elm$core$String$replace,
						'\t',
						'',
						A3($elm$core$String$replace, '\n', '', string))))));
	if ($elm$core$String$length(normalized) !== 32) {
		return $elm$core$Result$Err($TSFoster$elm_uuid$UUID$WrongLength);
	} else {
		var _v0 = A2(
			$elm$core$List$filterMap,
			$TSFoster$elm_uuid$UUID$toNibbleValue,
			$elm$core$String$toList(normalized));
		if ((((((((((((((((((((((((((((((((_v0.b && _v0.b.b) && _v0.b.b.b) && _v0.b.b.b.b) && _v0.b.b.b.b.b) && _v0.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && _v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b) && (!_v0.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b.b)) {
			var a1 = _v0.a;
			var _v1 = _v0.b;
			var a2 = _v1.a;
			var _v2 = _v1.b;
			var a3 = _v2.a;
			var _v3 = _v2.b;
			var a4 = _v3.a;
			var _v4 = _v3.b;
			var a5 = _v4.a;
			var _v5 = _v4.b;
			var a6 = _v5.a;
			var _v6 = _v5.b;
			var a7 = _v6.a;
			var _v7 = _v6.b;
			var a8 = _v7.a;
			var _v8 = _v7.b;
			var b1 = _v8.a;
			var _v9 = _v8.b;
			var b2 = _v9.a;
			var _v10 = _v9.b;
			var b3 = _v10.a;
			var _v11 = _v10.b;
			var b4 = _v11.a;
			var _v12 = _v11.b;
			var b5 = _v12.a;
			var _v13 = _v12.b;
			var b6 = _v13.a;
			var _v14 = _v13.b;
			var b7 = _v14.a;
			var _v15 = _v14.b;
			var b8 = _v15.a;
			var _v16 = _v15.b;
			var c1 = _v16.a;
			var _v17 = _v16.b;
			var c2 = _v17.a;
			var _v18 = _v17.b;
			var c3 = _v18.a;
			var _v19 = _v18.b;
			var c4 = _v19.a;
			var _v20 = _v19.b;
			var c5 = _v20.a;
			var _v21 = _v20.b;
			var c6 = _v21.a;
			var _v22 = _v21.b;
			var c7 = _v22.a;
			var _v23 = _v22.b;
			var c8 = _v23.a;
			var _v24 = _v23.b;
			var d1 = _v24.a;
			var _v25 = _v24.b;
			var d2 = _v25.a;
			var _v26 = _v25.b;
			var d3 = _v26.a;
			var _v27 = _v26.b;
			var d4 = _v27.a;
			var _v28 = _v27.b;
			var d5 = _v28.a;
			var _v29 = _v28.b;
			var d6 = _v29.a;
			var _v30 = _v29.b;
			var d7 = _v30.a;
			var _v31 = _v30.b;
			var d8 = _v31.a;
			return A4(
				$TSFoster$elm_uuid$UUID$fromInt32s,
				A8($TSFoster$elm_uuid$UUID$nibbleValuesToU32, a1, a2, a3, a4, a5, a6, a7, a8),
				A8($TSFoster$elm_uuid$UUID$nibbleValuesToU32, b1, b2, b3, b4, b5, b6, b7, b8),
				A8($TSFoster$elm_uuid$UUID$nibbleValuesToU32, c1, c2, c3, c4, c5, c6, c7, c8),
				A8($TSFoster$elm_uuid$UUID$nibbleValuesToU32, d1, d2, d3, d4, d5, d6, d7, d8));
		} else {
			return $elm$core$Result$Err($TSFoster$elm_uuid$UUID$WrongFormat);
		}
	}
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Converter$reportError = _Platform_outgoingPort('reportError', $elm$json$Json$Encode$string);
var $author$project$Converter$reportInfo = _Platform_outgoingPort('reportInfo', $elm$json$Json$Encode$string);
var $TSFoster$elm_uuid$UUID$Seeds = F4(
	function (seed1, seed2, seed3, seed4) {
		return {seed1: seed1, seed2: seed2, seed3: seed3, seed4: seed4};
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$maxInt = 2147483647;
var $elm$random$Random$minInt = -2147483648;
var $TSFoster$elm_uuid$UUID$randomU32 = A2(
	$elm$random$Random$map,
	$TSFoster$elm_uuid$UUID$forceUnsigned,
	A2($elm$random$Random$int, $elm$random$Random$minInt, $elm$random$Random$maxInt));
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $TSFoster$elm_uuid$UUID$toVariant1 = function (_v0) {
	var a = _v0.a;
	var b = _v0.b;
	var c = _v0.c;
	var d = _v0.d;
	return A4(
		$TSFoster$elm_uuid$UUID$UUID,
		a,
		b,
		$TSFoster$elm_uuid$UUID$forceUnsigned(2147483648 | (1073741823 & c)),
		d);
};
var $TSFoster$elm_uuid$UUID$toVersion = F2(
	function (v, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		var c = _v0.c;
		var d = _v0.d;
		return A4(
			$TSFoster$elm_uuid$UUID$UUID,
			a,
			$TSFoster$elm_uuid$UUID$forceUnsigned((v << 12) | (4294905855 & b)),
			c,
			d);
	});
var $TSFoster$elm_uuid$UUID$step = function (s) {
	var _v0 = A2($elm$random$Random$step, $TSFoster$elm_uuid$UUID$randomU32, s.seed4);
	var int4 = _v0.a;
	var seed4 = _v0.b;
	var _v1 = A2($elm$random$Random$step, $TSFoster$elm_uuid$UUID$randomU32, s.seed3);
	var int3 = _v1.a;
	var seed3 = _v1.b;
	var _v2 = A2($elm$random$Random$step, $TSFoster$elm_uuid$UUID$randomU32, s.seed2);
	var int2 = _v2.a;
	var seed2 = _v2.b;
	var _v3 = A2($elm$random$Random$step, $TSFoster$elm_uuid$UUID$randomU32, s.seed1);
	var int1 = _v3.a;
	var seed1 = _v3.b;
	var uuid = $TSFoster$elm_uuid$UUID$toVariant1(
		A2(
			$TSFoster$elm_uuid$UUID$toVersion,
			4,
			A4($TSFoster$elm_uuid$UUID$UUID, int1, int2, int3, int4)));
	return _Utils_Tuple2(
		uuid,
		A4($TSFoster$elm_uuid$UUID$Seeds, seed1, seed2, seed3, seed4));
};
var $author$project$Converter$init = function (flags) {
	var validMe = $bellroy$elm_email$Email$fromString(flags.me);
	var seeds = {
		seed1: $elm$random$Random$initialSeed(flags.seed1),
		seed2: $elm$random$Random$initialSeed(flags.seed2),
		seed3: $elm$random$Random$initialSeed(flags.seed3),
		seed4: $elm$random$Random$initialSeed(flags.seed4)
	};
	var project = $TSFoster$elm_uuid$UUID$fromString(flags.project);
	var _v0 = _Utils_Tuple2(validMe, project);
	if (_v0.a.$ === 'Just') {
		if (_v0.b.$ === 'Ok') {
			var project_ = _v0.b.a;
			return _Utils_Tuple2(
				{
					from: _List_Nil,
					me: flags.me,
					people: $elm$core$Dict$empty,
					project: project_,
					seeds: seeds,
					stage: $author$project$Converter$OriginalS,
					time: $elm$time$Time$millisToPosix(flags.time),
					to: $elm$core$Dict$empty
				},
				$author$project$Converter$reportInfo('Converter Initialized'));
		} else {
			var fakeuuid = function () {
				var _v1 = $TSFoster$elm_uuid$UUID$step(seeds);
				var uuid = _v1.a;
				return uuid;
			}();
			return _Utils_Tuple2(
				{
					from: _List_Nil,
					me: flags.me,
					people: $elm$core$Dict$empty,
					project: fakeuuid,
					seeds: seeds,
					stage: $author$project$Converter$OriginalS,
					time: $elm$time$Time$millisToPosix(0),
					to: $elm$core$Dict$empty
				},
				$author$project$Converter$reportError('The project flag is an invalid UUID.'));
		}
	} else {
		if (_v0.b.$ === 'Ok') {
			var _v2 = _v0.a;
			var project_ = _v0.b.a;
			return _Utils_Tuple2(
				{
					from: _List_Nil,
					me: 'nobody@example.com',
					people: $elm$core$Dict$empty,
					project: project_,
					seeds: seeds,
					stage: $author$project$Converter$OriginalS,
					time: $elm$time$Time$millisToPosix(0),
					to: $elm$core$Dict$empty
				},
				$author$project$Converter$reportError('The person flag is not a valid email address.'));
		} else {
			var _v3 = _v0.a;
			var fakeuuid = function () {
				var _v4 = $TSFoster$elm_uuid$UUID$step(seeds);
				var uuid = _v4.a;
				return uuid;
			}();
			return _Utils_Tuple2(
				{
					from: _List_Nil,
					me: 'nobody@example.com',
					people: $elm$core$Dict$empty,
					project: fakeuuid,
					seeds: seeds,
					stage: $author$project$Converter$OriginalS,
					time: $elm$time$Time$millisToPosix(0),
					to: $elm$core$Dict$empty
				},
				$author$project$Converter$reportError('The project and person flags are invalid.'));
		}
	}
};
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Converter$ReceivedDativeForms = function (a) {
	return {$: 'ReceivedDativeForms', a: a};
};
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $author$project$Converter$receivedDativeForms = _Platform_incomingPort('receivedDativeForms', $elm$json$Json$Decode$value);
var $author$project$Converter$subscriptions = function (_v0) {
	return $author$project$Converter$receivedDativeForms($author$project$Converter$ReceivedDativeForms);
};
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $author$project$DativeTypes$DativeForm = function (id) {
	return function (uuid) {
		return function (transcription) {
			return function (phonetic_transcription) {
				return function (narrow_phonetic_transcription) {
					return function (morpheme_break) {
						return function (morpheme_gloss) {
							return function (comments) {
								return function (speaker_comments) {
									return function (grammaticality) {
										return function (date_elicited) {
											return function (datetime_entered) {
												return function (datetime_modified) {
													return function (syntactic_category_string) {
														return function (morpheme_break_ids) {
															return function (morpheme_gloss_ids) {
																return function (break_gloss_category) {
																	return function (syntax) {
																		return function (semantics) {
																			return function (status) {
																				return function (elicitor) {
																					return function (enterer) {
																						return function (modifier) {
																							return function (verifier) {
																								return function (speaker) {
																									return function (elicitation_method) {
																										return function (syntactic_category) {
																											return function (source) {
																												return function (translations) {
																													return function (tags) {
																														return function (files) {
																															return {break_gloss_category: break_gloss_category, comments: comments, date_elicited: date_elicited, datetime_entered: datetime_entered, datetime_modified: datetime_modified, elicitation_method: elicitation_method, elicitor: elicitor, enterer: enterer, files: files, grammaticality: grammaticality, id: id, modifier: modifier, morpheme_break: morpheme_break, morpheme_break_ids: morpheme_break_ids, morpheme_gloss: morpheme_gloss, morpheme_gloss_ids: morpheme_gloss_ids, narrow_phonetic_transcription: narrow_phonetic_transcription, phonetic_transcription: phonetic_transcription, semantics: semantics, source: source, speaker: speaker, speaker_comments: speaker_comments, status: status, syntactic_category: syntactic_category, syntactic_category_string: syntactic_category_string, syntax: syntax, tags: tags, transcription: transcription, translations: translations, uuid: uuid, verifier: verifier};
																														};
																													};
																												};
																											};
																										};
																									};
																								};
																							};
																						};
																					};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm_community$json_extra$Json$Decode$Extra$andMap = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$core$Basics$round = _Basics_round;
var $elm$core$String$toFloat = _String_toFloat;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs = A2(
	$elm$parser$Parser$andThen,
	function (str) {
		if ($elm$core$String$length(str) <= 9) {
			var _v0 = $elm$core$String$toFloat('0.' + str);
			if (_v0.$ === 'Just') {
				var floatVal = _v0.a;
				return $elm$parser$Parser$succeed(
					$elm$core$Basics$round(floatVal * 1000));
			} else {
				return $elm$parser$Parser$problem('Invalid float: \"' + (str + '\"'));
			}
		} else {
			return $elm$parser$Parser$problem(
				'Expected at most 9 digits, but got ' + $elm$core$String$fromInt(
					$elm$core$String$length(str)));
		}
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$chompWhile($elm$core$Char$isDigit)));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts = F6(
	function (monthYearDayMs, hour, minute, second, ms, utcOffsetMinutes) {
		return $elm$time$Time$millisToPosix((((monthYearDayMs + (((hour * 60) * 60) * 1000)) + (((minute - utcOffsetMinutes) * 60) * 1000)) + (second * 1000)) + ms);
	});
var $elm$core$String$append = _String_append;
var $elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return $elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					$elm$parser$Parser$Advanced$Bad,
					false,
					A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					$elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$core$String$toInt = _String_toInt;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt = function (quantity) {
	var helper = function (str) {
		if (_Utils_eq(
			$elm$core$String$length(str),
			quantity)) {
			var _v0 = $elm$core$String$toInt(str);
			if (_v0.$ === 'Just') {
				var intVal = _v0.a;
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$Done,
					$elm$parser$Parser$succeed(intVal));
			} else {
				return $elm$parser$Parser$problem('Invalid integer: \"' + (str + '\"'));
			}
		} else {
			return A2(
				$elm$parser$Parser$map,
				function (nextChar) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$String$append, str, nextChar));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf($elm$core$Char$isDigit)));
		}
	};
	return A2($elm$parser$Parser$loop, '', helper);
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear = 1970;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay = function (day) {
	return $elm$parser$Parser$problem(
		'Invalid day: ' + $elm$core$String$fromInt(day));
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear = function (year) {
	return (!A2($elm$core$Basics$modBy, 4, year)) && ((!(!A2($elm$core$Basics$modBy, 100, year))) || (!A2($elm$core$Basics$modBy, 400, year)));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore = function (y1) {
	var y = y1 - 1;
	return (((y / 4) | 0) - ((y / 100) | 0)) + ((y / 400) | 0);
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay = 86400000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear = 31536000000;
var $rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay = function (_v0) {
	var year = _v0.a;
	var month = _v0.b;
	var dayInMonth = _v0.c;
	if (dayInMonth < 0) {
		return $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth);
	} else {
		var succeedWith = function (extraMs) {
			var yearMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerYear * (year - $rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear);
			var days = ((month < 3) || (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year))) ? (dayInMonth - 1) : dayInMonth;
			var dayMs = $rtfeldman$elm_iso8601_date_strings$Iso8601$msPerDay * (days + ($rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore(year) - $rtfeldman$elm_iso8601_date_strings$Iso8601$leapYearsBefore($rtfeldman$elm_iso8601_date_strings$Iso8601$epochYear)));
			return $elm$parser$Parser$succeed((extraMs + yearMs) + dayMs);
		};
		switch (month) {
			case 1:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(0);
			case 2:
				return ((dayInMonth > 29) || ((dayInMonth === 29) && (!$rtfeldman$elm_iso8601_date_strings$Iso8601$isLeapYear(year)))) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(2678400000);
			case 3:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(5097600000);
			case 4:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(7776000000);
			case 5:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(10368000000);
			case 6:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(13046400000);
			case 7:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(15638400000);
			case 8:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(18316800000);
			case 9:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(20995200000);
			case 10:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(23587200000);
			case 11:
				return (dayInMonth > 30) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(26265600000);
			case 12:
				return (dayInMonth > 31) ? $rtfeldman$elm_iso8601_date_strings$Iso8601$invalidDay(dayInMonth) : succeedWith(28857600000);
			default:
				return $elm$parser$Parser$problem(
					'Invalid month: \"' + ($elm$core$String$fromInt(month) + '\"'));
		}
	}
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs = A2(
	$elm$parser$Parser$andThen,
	$rtfeldman$elm_iso8601_date_strings$Iso8601$yearMonthDay,
	A2(
		$elm$parser$Parser$keeper,
		A2(
			$elm$parser$Parser$keeper,
			A2(
				$elm$parser$Parser$keeper,
				$elm$parser$Parser$succeed(
					F3(
						function (year, month, day) {
							return _Utils_Tuple3(year, month, day);
						})),
				$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(4)),
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$ignorer,
							$elm$parser$Parser$succeed($elm$core$Basics$identity),
							$elm$parser$Parser$symbol('-')),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
					]))),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$ignorer,
						$elm$parser$Parser$succeed($elm$core$Basics$identity),
						$elm$parser$Parser$symbol('-')),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
				]))));
var $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes = function () {
	var utcOffsetMinutesFromParts = F3(
		function (multiplier, hours, minutes) {
			return (multiplier * (hours * 60)) + minutes;
		});
	return A2(
		$elm$parser$Parser$keeper,
		$elm$parser$Parser$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return 0;
					},
					$elm$parser$Parser$symbol('Z')),
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							$elm$parser$Parser$succeed(utcOffsetMinutesFromParts),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$map,
										function (_v1) {
											return 1;
										},
										$elm$parser$Parser$symbol('+')),
										A2(
										$elm$parser$Parser$map,
										function (_v2) {
											return -1;
										},
										$elm$parser$Parser$symbol('-'))
									]))),
						$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$ignorer,
									$elm$parser$Parser$succeed($elm$core$Basics$identity),
									$elm$parser$Parser$symbol(':')),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
								$elm$parser$Parser$succeed(0)
							]))),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$end)
				])));
}();
var $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601 = A2(
	$elm$parser$Parser$andThen,
	function (datePart) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$keeper,
					A2(
						$elm$parser$Parser$keeper,
						A2(
							$elm$parser$Parser$keeper,
							A2(
								$elm$parser$Parser$keeper,
								A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed(
											$rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts(datePart)),
										$elm$parser$Parser$symbol('T')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
								$elm$parser$Parser$oneOf(
									_List_fromArray(
										[
											A2(
											$elm$parser$Parser$keeper,
											A2(
												$elm$parser$Parser$ignorer,
												$elm$parser$Parser$succeed($elm$core$Basics$identity),
												$elm$parser$Parser$symbol(':')),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
											$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)
										]))),
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$keeper,
										A2(
											$elm$parser$Parser$ignorer,
											$elm$parser$Parser$succeed($elm$core$Basics$identity),
											$elm$parser$Parser$symbol(':')),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2)),
										$rtfeldman$elm_iso8601_date_strings$Iso8601$paddedInt(2),
										$elm$parser$Parser$succeed(0)
									]))),
						$elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									A2(
									$elm$parser$Parser$keeper,
									A2(
										$elm$parser$Parser$ignorer,
										$elm$parser$Parser$succeed($elm$core$Basics$identity),
										$elm$parser$Parser$symbol('.')),
									$rtfeldman$elm_iso8601_date_strings$Iso8601$fractionsOfASecondInMs),
									$elm$parser$Parser$succeed(0)
								]))),
					A2($elm$parser$Parser$ignorer, $rtfeldman$elm_iso8601_date_strings$Iso8601$utcOffsetInMinutes, $elm$parser$Parser$end)),
					A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(
						A6($rtfeldman$elm_iso8601_date_strings$Iso8601$fromParts, datePart, 0, 0, 0, 0, 0)),
					$elm$parser$Parser$end)
				]));
	},
	$rtfeldman$elm_iso8601_date_strings$Iso8601$monthYearDayInMs);
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime = function (str) {
	return A2($elm$parser$Parser$run, $rtfeldman$elm_iso8601_date_strings$Iso8601$iso8601, str);
};
var $elm_community$json_extra$Json$Decode$Extra$datetime = A2(
	$elm$json$Json$Decode$andThen,
	function (dateString) {
		var _v0 = $rtfeldman$elm_iso8601_date_strings$Iso8601$toTime(dateString);
		if (_v0.$ === 'Ok') {
			var v = _v0.a;
			return $elm$json$Json$Decode$succeed(v);
		} else {
			return $elm$json$Json$Decode$fail('Expecting an ISO-8601 formatted date+time string');
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$DativeTypes$DativeNamed = F2(
	function (id, name) {
		return {id: id, name: name};
	});
var $author$project$DativeTypes$dativeNamedDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$DativeTypes$DativeNamed,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string));
var $author$project$DativeTypes$DativePerson = F4(
	function (id, first_name, last_name, role) {
		return {first_name: first_name, id: id, last_name: last_name, role: role};
	});
var $elm$json$Json$Decode$map4 = _Json_map4;
var $author$project$DativeTypes$dativePersonDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$DativeTypes$DativePerson,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'first_name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'last_name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'role', $elm$json$Json$Decode$string));
var $author$project$DativeTypes$DativeSpeaker = F4(
	function (id, first_name, last_name, dialect) {
		return {dialect: dialect, first_name: first_name, id: id, last_name: last_name};
	});
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$DativeTypes$dativeSpeakerDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$DativeTypes$DativeSpeaker,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'first_name', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'last_name', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'dialect',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)));
var $author$project$DativeTypes$DativeTranslation = F3(
	function (id, transcription, grammaticality) {
		return {grammaticality: grammaticality, id: id, transcription: transcription};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$DativeTypes$dativeTranslationDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$DativeTypes$DativeTranslation,
	A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
	A2($elm$json$Json$Decode$field, 'transcription', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'grammaticality', $elm$json$Json$Decode$string));
var $TSFoster$elm_uuid$UUID$stringToJsonDecoder = function (string) {
	var _v0 = $TSFoster$elm_uuid$UUID$fromString(string);
	if (_v0.$ === 'Ok') {
		var uuid = _v0.a;
		return $elm$json$Json$Decode$succeed(uuid);
	} else {
		switch (_v0.a.$) {
			case 'WrongFormat':
				var _v1 = _v0.a;
				return $elm$json$Json$Decode$fail('UUID is in wrong format');
			case 'WrongLength':
				var _v2 = _v0.a;
				return $elm$json$Json$Decode$fail('UUID is wrong length');
			case 'UnsupportedVariant':
				var _v3 = _v0.a;
				return $elm$json$Json$Decode$fail('UUID is an unsupported variant');
			case 'IsNil':
				var _v4 = _v0.a;
				return $elm$json$Json$Decode$fail('UUID is nil');
			default:
				var _v5 = _v0.a;
				return $elm$json$Json$Decode$fail('UUID is not properly versioned');
		}
	}
};
var $TSFoster$elm_uuid$UUID$jsonDecoder = A2($elm$json$Json$Decode$andThen, $TSFoster$elm_uuid$UUID$stringToJsonDecoder, $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$DativeTypes$DativeSubToken = F3(
	function (id, code1, code2) {
		return {code1: code1, code2: code2, id: id};
	});
var $elm$json$Json$Decode$index = _Json_decodeIndex;
var $elm$json$Json$Decode$maybe = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder),
				$elm$json$Json$Decode$succeed($elm$core$Maybe$Nothing)
			]));
};
var $author$project$DativeTypes$subTokenDecoder = A4(
	$elm$json$Json$Decode$map3,
	$author$project$DativeTypes$DativeSubToken,
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$index, 0, $elm$json$Json$Decode$int)),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$index, 1, $elm$json$Json$Decode$string)),
	$elm$json$Json$Decode$maybe(
		A2($elm$json$Json$Decode$index, 2, $elm$json$Json$Decode$string)));
var $author$project$DativeTypes$tokenDecoder = $elm$json$Json$Decode$list($author$project$DativeTypes$subTokenDecoder);
var $author$project$DativeTypes$decoder = A2(
	$elm_community$json_extra$Json$Decode$Extra$andMap,
	A2(
		$elm$json$Json$Decode$field,
		'files',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)),
	A2(
		$elm_community$json_extra$Json$Decode$Extra$andMap,
		A2(
			$elm$json$Json$Decode$field,
			'tags',
			$elm$json$Json$Decode$list($author$project$DativeTypes$dativeNamedDecoder)),
		A2(
			$elm_community$json_extra$Json$Decode$Extra$andMap,
			A2(
				$elm$json$Json$Decode$field,
				'translations',
				$elm$json$Json$Decode$list($author$project$DativeTypes$dativeTranslationDecoder)),
			A2(
				$elm_community$json_extra$Json$Decode$Extra$andMap,
				A2(
					$elm$json$Json$Decode$field,
					'source',
					$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
				A2(
					$elm_community$json_extra$Json$Decode$Extra$andMap,
					A2(
						$elm$json$Json$Decode$field,
						'syntactic_category',
						$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativeNamedDecoder)),
					A2(
						$elm_community$json_extra$Json$Decode$Extra$andMap,
						A2(
							$elm$json$Json$Decode$field,
							'elicitation_method',
							$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativeNamedDecoder)),
						A2(
							$elm_community$json_extra$Json$Decode$Extra$andMap,
							A2(
								$elm$json$Json$Decode$field,
								'speaker',
								$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativeSpeakerDecoder)),
							A2(
								$elm_community$json_extra$Json$Decode$Extra$andMap,
								A2(
									$elm$json$Json$Decode$field,
									'verifier',
									$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativePersonDecoder)),
								A2(
									$elm_community$json_extra$Json$Decode$Extra$andMap,
									A2(
										$elm$json$Json$Decode$field,
										'modifier',
										$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativePersonDecoder)),
									A2(
										$elm_community$json_extra$Json$Decode$Extra$andMap,
										A2(
											$elm$json$Json$Decode$field,
											'enterer',
											$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativePersonDecoder)),
										A2(
											$elm_community$json_extra$Json$Decode$Extra$andMap,
											A2(
												$elm$json$Json$Decode$field,
												'elicitor',
												$elm$json$Json$Decode$nullable($author$project$DativeTypes$dativePersonDecoder)),
											A2(
												$elm_community$json_extra$Json$Decode$Extra$andMap,
												A2($elm$json$Json$Decode$field, 'status', $elm$json$Json$Decode$string),
												A2(
													$elm_community$json_extra$Json$Decode$Extra$andMap,
													A2($elm$json$Json$Decode$field, 'semantics', $elm$json$Json$Decode$string),
													A2(
														$elm_community$json_extra$Json$Decode$Extra$andMap,
														A2($elm$json$Json$Decode$field, 'syntax', $elm$json$Json$Decode$string),
														A2(
															$elm_community$json_extra$Json$Decode$Extra$andMap,
															A2(
																$elm$json$Json$Decode$field,
																'break_gloss_category',
																$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
															A2(
																$elm_community$json_extra$Json$Decode$Extra$andMap,
																A2(
																	$elm$json$Json$Decode$field,
																	'morpheme_gloss_ids',
																	$elm$json$Json$Decode$nullable(
																		$elm$json$Json$Decode$list($author$project$DativeTypes$tokenDecoder))),
																A2(
																	$elm_community$json_extra$Json$Decode$Extra$andMap,
																	A2(
																		$elm$json$Json$Decode$field,
																		'morpheme_break_ids',
																		$elm$json$Json$Decode$nullable(
																			$elm$json$Json$Decode$list($author$project$DativeTypes$tokenDecoder))),
																	A2(
																		$elm_community$json_extra$Json$Decode$Extra$andMap,
																		A2(
																			$elm$json$Json$Decode$field,
																			'syntactic_category_string',
																			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
																		A2(
																			$elm_community$json_extra$Json$Decode$Extra$andMap,
																			A2($elm$json$Json$Decode$field, 'datetime_modified', $elm_community$json_extra$Json$Decode$Extra$datetime),
																			A2(
																				$elm_community$json_extra$Json$Decode$Extra$andMap,
																				A2($elm$json$Json$Decode$field, 'datetime_entered', $elm_community$json_extra$Json$Decode$Extra$datetime),
																				A2(
																					$elm_community$json_extra$Json$Decode$Extra$andMap,
																					A2(
																						$elm$json$Json$Decode$field,
																						'date_elicited',
																						$elm$json$Json$Decode$nullable($elm_community$json_extra$Json$Decode$Extra$datetime)),
																					A2(
																						$elm_community$json_extra$Json$Decode$Extra$andMap,
																						A2($elm$json$Json$Decode$field, 'grammaticality', $elm$json$Json$Decode$string),
																						A2(
																							$elm_community$json_extra$Json$Decode$Extra$andMap,
																							A2($elm$json$Json$Decode$field, 'speaker_comments', $elm$json$Json$Decode$string),
																							A2(
																								$elm_community$json_extra$Json$Decode$Extra$andMap,
																								A2($elm$json$Json$Decode$field, 'comments', $elm$json$Json$Decode$string),
																								A2(
																									$elm_community$json_extra$Json$Decode$Extra$andMap,
																									A2($elm$json$Json$Decode$field, 'morpheme_gloss', $elm$json$Json$Decode$string),
																									A2(
																										$elm_community$json_extra$Json$Decode$Extra$andMap,
																										A2($elm$json$Json$Decode$field, 'morpheme_break', $elm$json$Json$Decode$string),
																										A2(
																											$elm_community$json_extra$Json$Decode$Extra$andMap,
																											A2($elm$json$Json$Decode$field, 'narrow_phonetic_transcription', $elm$json$Json$Decode$string),
																											A2(
																												$elm_community$json_extra$Json$Decode$Extra$andMap,
																												A2($elm$json$Json$Decode$field, 'phonetic_transcription', $elm$json$Json$Decode$string),
																												A2(
																													$elm_community$json_extra$Json$Decode$Extra$andMap,
																													A2($elm$json$Json$Decode$field, 'transcription', $elm$json$Json$Decode$string),
																													A2(
																														$elm_community$json_extra$Json$Decode$Extra$andMap,
																														A2($elm$json$Json$Decode$field, 'UUID', $TSFoster$elm_uuid$UUID$jsonDecoder),
																														A2(
																															$elm_community$json_extra$Json$Decode$Extra$andMap,
																															A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$int),
																															$elm$json$Json$Decode$succeed($author$project$DativeTypes$DativeForm))))))))))))))))))))))))))))))));
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Menkayonta$addRev = F2(
	function (rev, fields) {
		return A2(
			$elm$core$Maybe$withDefault,
			$elm$json$Json$Encode$object(fields),
			A2(
				$elm$core$Maybe$map,
				$elm$json$Json$Encode$object,
				A2(
					$elm$core$Maybe$map,
					function (rev_) {
						return A2($elm$core$List$cons, rev_, fields);
					},
					A2(
						$elm$core$Maybe$map,
						function (rev_) {
							return _Utils_Tuple2(
								'_rev',
								$elm$json$Json$Encode$string(rev_));
						},
						rev))));
	});
var $elm$url$Url$Builder$Relative = {$: 'Relative'};
var $elm$url$Url$Builder$rootToPrePath = function (root) {
	switch (root.$) {
		case 'Absolute':
			return '/';
		case 'Relative':
			return '';
		default:
			var prePath = root.a;
			return prePath + '/';
	}
};
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$custom = F4(
	function (root, pathSegments, parameters, maybeFragment) {
		var fragmentless = _Utils_ap(
			$elm$url$Url$Builder$rootToPrePath(root),
			_Utils_ap(
				A2($elm$core$String$join, '/', pathSegments),
				$elm$url$Url$Builder$toQuery(parameters)));
		if (maybeFragment.$ === 'Nothing') {
			return fragmentless;
		} else {
			var fragment = maybeFragment.a;
			return fragmentless + ('#' + fragment);
		}
	});
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				$elm$core$String$repeat,
				n - $elm$core$String$length(string),
				$elm$core$String$fromChar(_char)),
			string);
	});
var $elm$core$String$fromList = _String_fromList;
var $TSFoster$elm_uuid$UUID$toHex = F2(
	function (acc, _int) {
		toHex:
		while (true) {
			if (!_int) {
				return $elm$core$String$fromList(acc);
			} else {
				var _char = function () {
					var _v0 = 15 & _int;
					switch (_v0) {
						case 0:
							return _Utils_chr('0');
						case 1:
							return _Utils_chr('1');
						case 2:
							return _Utils_chr('2');
						case 3:
							return _Utils_chr('3');
						case 4:
							return _Utils_chr('4');
						case 5:
							return _Utils_chr('5');
						case 6:
							return _Utils_chr('6');
						case 7:
							return _Utils_chr('7');
						case 8:
							return _Utils_chr('8');
						case 9:
							return _Utils_chr('9');
						case 10:
							return _Utils_chr('a');
						case 11:
							return _Utils_chr('b');
						case 12:
							return _Utils_chr('c');
						case 13:
							return _Utils_chr('d');
						case 14:
							return _Utils_chr('e');
						default:
							return _Utils_chr('f');
					}
				}();
				var $temp$acc = A2($elm$core$List$cons, _char, acc),
					$temp$int = _int >>> 4;
				acc = $temp$acc;
				_int = $temp$int;
				continue toHex;
			}
		}
	});
var $TSFoster$elm_uuid$UUID$toStringWith = F2(
	function (sep, _v0) {
		var a = _v0.a;
		var b = _v0.b;
		var c = _v0.c;
		var d = _v0.d;
		return _Utils_ap(
			A3(
				$elm$core$String$padLeft,
				8,
				_Utils_chr('0'),
				A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, a)),
			_Utils_ap(
				sep,
				_Utils_ap(
					A3(
						$elm$core$String$padLeft,
						4,
						_Utils_chr('0'),
						A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, b >>> 16)),
					_Utils_ap(
						sep,
						_Utils_ap(
							A3(
								$elm$core$String$padLeft,
								4,
								_Utils_chr('0'),
								A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, 65535 & b)),
							_Utils_ap(
								sep,
								_Utils_ap(
									A3(
										$elm$core$String$padLeft,
										4,
										_Utils_chr('0'),
										A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, c >>> 16)),
									_Utils_ap(
										sep,
										_Utils_ap(
											A3(
												$elm$core$String$padLeft,
												4,
												_Utils_chr('0'),
												A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, 65535 & c)),
											A3(
												$elm$core$String$padLeft,
												8,
												_Utils_chr('0'),
												A2($TSFoster$elm_uuid$UUID$toHex, _List_Nil, d)))))))))));
	});
var $TSFoster$elm_uuid$UUID$toString = $TSFoster$elm_uuid$UUID$toStringWith('-');
var $author$project$Menkayonta$docIdToList = function (docid) {
	if (docid.$ === 'PersonId') {
		var id = docid.a;
		return A2(
			$elm$core$List$map,
			$elm$url$Url$percentEncode,
			_List_fromArray(
				['person', id]));
	} else {
		var uuid = docid.a;
		return A2(
			$elm$core$List$map,
			$elm$url$Url$percentEncode,
			_List_fromArray(
				[
					'interlinear',
					$TSFoster$elm_uuid$UUID$toString(uuid)
				]));
	}
};
var $author$project$Menkayonta$descriptionIdToString = function (descriptionid) {
	var path = A2(
		$elm$core$List$map,
		$elm$url$Url$percentEncode,
		_Utils_ap(
			$author$project$Menkayonta$docIdToList(descriptionid.docid),
			_List_fromArray(
				['description', descriptionid.kind])));
	return A4($elm$url$Url$Builder$custom, $elm$url$Url$Builder$Relative, path, _List_Nil, descriptionid.fragment);
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Menkayonta$descriptionEncoder = function (description) {
	return A2(
		$author$project$Menkayonta$addRev,
		description.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$descriptionIdToString(description.id))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(description.version)),
				_Utils_Tuple2(
				'value',
				$elm$json$Json$Encode$string(description.value))
			]));
};
var $author$project$Menkayonta$InterlinearId = function (a) {
	return {$: 'InterlinearId', a: a};
};
var $author$project$Menkayonta$annEncoder = function (ann) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'breaks',
				$elm$json$Json$Encode$string(ann.breaks)),
				_Utils_Tuple2(
				'glosses',
				$elm$json$Json$Encode$string(ann.glosses)),
				_Utils_Tuple2(
				'phonemic',
				$elm$json$Json$Encode$string(ann.phonemic)),
				_Utils_Tuple2(
				'judgment',
				$elm$json$Json$Encode$string(ann.judgment))
			]));
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $elm$url$Url$Builder$relative = F2(
	function (pathSegments, parameters) {
		return _Utils_ap(
			A2($elm$core$String$join, '/', pathSegments),
			$elm$url$Url$Builder$toQuery(parameters));
	});
var $author$project$Menkayonta$docIdToString = function (docid) {
	return A2(
		$elm$url$Url$Builder$relative,
		$author$project$Menkayonta$docIdToList(docid),
		_List_Nil);
};
var $author$project$Menkayonta$translationEncoder = function (tr) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'translation',
				$elm$json$Json$Encode$string(tr.translation)),
				_Utils_Tuple2(
				'judgment',
				$elm$json$Json$Encode$string(tr.judgment))
			]));
};
var $author$project$Menkayonta$interlinearEncoder = function (_int) {
	return A2(
		$author$project$Menkayonta$addRev,
		_int.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$docIdToString(
						$author$project$Menkayonta$InterlinearId(_int.id)))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(_int.version)),
				_Utils_Tuple2(
				'text',
				$elm$json$Json$Encode$string(_int.text)),
				_Utils_Tuple2(
				'ann',
				$author$project$Menkayonta$annEncoder(_int.ann)),
				_Utils_Tuple2(
				'translations',
				A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $author$project$Menkayonta$translationEncoder, _int.translations))
			]));
};
var $author$project$Menkayonta$docIdToSimpleString = function (docid) {
	if (docid.$ === 'PersonId') {
		var id = docid.a;
		return id;
	} else {
		var uuid = docid.a;
		return $TSFoster$elm_uuid$UUID$toString(uuid);
	}
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $author$project$Menkayonta$modificationIdToString = function (modid) {
	var path = A2(
		$elm$core$List$map,
		$elm$url$Url$percentEncode,
		_Utils_ap(
			$author$project$Menkayonta$docIdToList(modid.docid),
			_List_fromArray(
				[
					'modification',
					modid.kind,
					$elm$core$String$fromInt(
					$elm$time$Time$posixToMillis(modid.time)),
					$author$project$Menkayonta$docIdToSimpleString(modid.person)
				])));
	return A4($elm$url$Url$Builder$custom, $elm$url$Url$Builder$Relative, path, _List_Nil, modid.fragment);
};
var $author$project$Menkayonta$modificationEncoder = function (modification) {
	return A2(
		$author$project$Menkayonta$addRev,
		modification.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$modificationIdToString(modification.id))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(modification.version)),
				_Utils_Tuple2(
				'comment',
				$elm$json$Json$Encode$string(modification.comment)),
				_Utils_Tuple2(
				'docversion',
				$elm$json$Json$Encode$int(modification.docversion)),
				_Utils_Tuple2('value', modification.value)
			]));
};
var $author$project$Menkayonta$PersonId = function (a) {
	return {$: 'PersonId', a: a};
};
var $author$project$Menkayonta$personEncoder = function (person) {
	return A2(
		$author$project$Menkayonta$addRev,
		person.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$docIdToString(
						$author$project$Menkayonta$PersonId(person.id)))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(person.version)),
				_Utils_Tuple2(
				'names',
				A3($elm$json$Json$Encode$dict, $elm$core$String$fromInt, $elm$json$Json$Encode$string, person.names))
			]));
};
var $author$project$Menkayonta$propertyIdToString = function (propertyid) {
	var path = A2(
		$elm$core$List$map,
		$elm$url$Url$percentEncode,
		_Utils_ap(
			$author$project$Menkayonta$docIdToList(propertyid.docid),
			_List_fromArray(
				['property', propertyid.kind, propertyid.value])));
	return A4($elm$url$Url$Builder$custom, $elm$url$Url$Builder$Relative, path, _List_Nil, propertyid.fragment);
};
var $author$project$Menkayonta$propertyEncoder = function (property) {
	return A2(
		$author$project$Menkayonta$addRev,
		property.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$propertyIdToString(property.id))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(property.version))
			]));
};
var $author$project$Menkayonta$tagIdToString = function (tagid) {
	var path = A2(
		$elm$core$List$map,
		$elm$url$Url$percentEncode,
		_Utils_ap(
			$author$project$Menkayonta$docIdToList(tagid.docid),
			_List_fromArray(
				['tag', tagid.kind])));
	return A4($elm$url$Url$Builder$custom, $elm$url$Url$Builder$Relative, path, _List_Nil, tagid.fragment);
};
var $author$project$Menkayonta$tagEncoder = function (tag) {
	return A2(
		$author$project$Menkayonta$addRev,
		tag.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$tagIdToString(tag.id))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(tag.version))
			]));
};
var $author$project$Menkayonta$utilityIdToString = function (utilityid) {
	var path = A2(
		$elm$core$List$map,
		$elm$url$Url$percentEncode,
		A2(
			$elm$core$List$cons,
			'utility',
			A2(
				$elm$core$List$cons,
				utilityid.kind,
				$author$project$Menkayonta$docIdToList(utilityid.docid))));
	return A4($elm$url$Url$Builder$custom, $elm$url$Url$Builder$Relative, path, _List_Nil, utilityid.fragment);
};
var $author$project$Menkayonta$utilityEncoder = function (utility) {
	return A2(
		$author$project$Menkayonta$addRev,
		utility.rev,
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					$author$project$Menkayonta$utilityIdToString(utility.id))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(utility.version)),
				_Utils_Tuple2('value', utility.value)
			]));
};
var $author$project$Menkayonta$encoder = function (value) {
	switch (value.$) {
		case 'MyPerson':
			var person = value.a;
			return $author$project$Menkayonta$personEncoder(person);
		case 'MyInterlinear':
			var interlinear = value.a;
			return $author$project$Menkayonta$interlinearEncoder(interlinear);
		case 'MyTag':
			var tag = value.a;
			return $author$project$Menkayonta$tagEncoder(tag);
		case 'MyProperty':
			var property = value.a;
			return $author$project$Menkayonta$propertyEncoder(property);
		case 'MyUtility':
			var utility = value.a;
			return $author$project$Menkayonta$utilityEncoder(utility);
		case 'MyModification':
			var modification = value.a;
			return $author$project$Menkayonta$modificationEncoder(modification);
		default:
			var description = value.a;
			return $author$project$Menkayonta$descriptionEncoder(description);
	}
};
var $author$project$Converter$Job = F2(
	function (project, payload) {
		return {payload: payload, project: project};
	});
var $author$project$Converter$jobDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Converter$Job,
	A2($elm$json$Json$Decode$field, 'project', $TSFoster$elm_uuid$UUID$jsonDecoder),
	A2($elm$json$Json$Decode$field, 'payload', $elm$json$Json$Decode$value));
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $TSFoster$elm_uuid$UUID$toValue = A2($elm$core$Basics$composeR, $TSFoster$elm_uuid$UUID$toString, $elm$json$Json$Encode$string);
var $author$project$Converter$jobEncoder = function (job) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'project',
				$TSFoster$elm_uuid$UUID$toValue(job.project)),
				_Utils_Tuple2('payload', job.payload)
			]));
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$Converter$Next = {$: 'Next'};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $author$project$Converter$next = function (msg) {
	return $elm$core$Platform$Cmd$batch(
		_List_fromArray(
			[
				A2(
				$elm$core$Task$perform,
				$elm$core$Basics$identity,
				$elm$core$Task$succeed($author$project$Converter$Next)),
				$author$project$Converter$reportInfo(msg)
			]));
};
var $author$project$Converter$DativeUtilityValue = F5(
	function (version, syntactic_category_string, morpheme_break_ids, morpheme_gloss_ids, break_gloss_category) {
		return {break_gloss_category: break_gloss_category, morpheme_break_ids: morpheme_break_ids, morpheme_gloss_ids: morpheme_gloss_ids, syntactic_category_string: syntactic_category_string, version: version};
	});
var $author$project$Converter$ElicitorS = {$: 'ElicitorS'};
var $author$project$Converter$EntererS = {$: 'EntererS'};
var $author$project$Converter$InterlinearS = {$: 'InterlinearS'};
var $author$project$Converter$ModifierS = {$: 'ModifierS'};
var $author$project$Converter$SpeakerS = {$: 'SpeakerS'};
var $author$project$Converter$UtilityS = {$: 'UtilityS'};
var $author$project$Converter$VerifierS = {$: 'VerifierS'};
var $author$project$Menkayonta$MyModification = function (a) {
	return {$: 'MyModification', a: a};
};
var $author$project$Menkayonta$MyModificationId = function (a) {
	return {$: 'MyModificationId', a: a};
};
var $author$project$Converter$constructModifier = function (_v0) {
	var kind = _v0.kind;
	var time = _v0.time;
	var docid = _v0.docid;
	var pid = _v0.pid;
	var json = _v0.json;
	return function (mod) {
		return {
			id: $author$project$Menkayonta$MyModificationId(mod.id),
			val: $author$project$Menkayonta$MyModification(mod)
		};
	}(
		{
			comment: 'bulk import',
			docversion: 0,
			id: {
				docid: docid,
				fragment: $elm$core$Maybe$Nothing,
				kind: kind,
				person: $author$project$Menkayonta$PersonId(pid),
				time: time
			},
			rev: $elm$core$Maybe$Nothing,
			value: json,
			version: 1
		});
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm_community$json_extra$Json$Encode$Extra$maybe = function (encoder) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$Maybe$map(encoder),
		$elm$core$Maybe$withDefault($elm$json$Json$Encode$null));
};
var $author$project$DativeTypes$dativeSubTokenEncoder = function (st) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$int, st.id)),
				_Utils_Tuple2(
				'code1',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, st.code1)),
				_Utils_Tuple2(
				'code2',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, st.code2))
			]));
};
var $author$project$DativeTypes$dativeTokenEncoder = function (sts) {
	return A2($elm$json$Json$Encode$list, $author$project$DativeTypes$dativeSubTokenEncoder, sts);
};
var $author$project$Converter$dativeUtilEncoder = function (du) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(du.version)),
				_Utils_Tuple2(
				'syntactic_category_string',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, du.syntactic_category_string)),
				_Utils_Tuple2(
				'morpheme_break_ids',
				A2(
					$elm_community$json_extra$Json$Encode$Extra$maybe,
					$elm$json$Json$Encode$list($author$project$DativeTypes$dativeTokenEncoder),
					du.morpheme_break_ids)),
				_Utils_Tuple2(
				'morpheme_gloss_ids',
				A2(
					$elm_community$json_extra$Json$Encode$Extra$maybe,
					$elm$json$Json$Encode$list($author$project$DativeTypes$dativeTokenEncoder),
					du.morpheme_gloss_ids)),
				_Utils_Tuple2(
				'break_gloss_category',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, du.break_gloss_category))
			]));
};
var $author$project$Menkayonta$MyPerson = function (a) {
	return {$: 'MyPerson', a: a};
};
var $author$project$Menkayonta$MyDescription = function (a) {
	return {$: 'MyDescription', a: a};
};
var $author$project$Menkayonta$MyDescriptionId = function (a) {
	return {$: 'MyDescriptionId', a: a};
};
var $author$project$Converter$constructDescription = F3(
	function (kind, value, docid) {
		return function (x) {
			return {
				id: $author$project$Menkayonta$MyDescriptionId(x.id),
				val: $author$project$Menkayonta$MyDescription(x)
			};
		}(
			{
				id: {docid: docid, fragment: $elm$core$Maybe$Nothing, kind: kind},
				rev: $elm$core$Maybe$Nothing,
				value: value,
				version: 1
			});
	});
var $author$project$Converter$constNonBlankDesc = F3(
	function (kind, c, docid) {
		return (!$elm$core$String$isEmpty(c)) ? $elm$core$Maybe$Just(
			A3($author$project$Converter$constructDescription, kind, c, docid)) : $elm$core$Maybe$Nothing;
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Menkayonta$MyProperty = function (a) {
	return {$: 'MyProperty', a: a};
};
var $author$project$Menkayonta$MyPropertyId = function (a) {
	return {$: 'MyPropertyId', a: a};
};
var $author$project$Converter$constructProperty = F3(
	function (kind, str, docid) {
		return function (x) {
			return {
				id: $author$project$Menkayonta$MyPropertyId(x.id),
				val: $author$project$Menkayonta$MyProperty(x)
			};
		}(
			{
				id: {docid: docid, fragment: $elm$core$Maybe$Nothing, kind: kind, value: str},
				rev: $elm$core$Maybe$Nothing,
				version: 1
			});
	});
var $author$project$Converter$maybeConstProp = F3(
	function (kind, str, docid) {
		return A2(
			$elm$core$Maybe$map,
			function (x) {
				return A3($author$project$Converter$constructProperty, kind, x, docid);
			},
			str);
	});
var $author$project$Converter$modDate = F4(
	function (kind, time, docid, pid) {
		return A2(
			$elm$core$Maybe$map,
			function (ed) {
				return $author$project$Converter$constructModifier(
					{docid: docid, json: $elm$json$Json$Encode$null, kind: kind, pid: pid, time: ed});
			},
			time);
	});
var $author$project$Menkayonta$MyDocId = function (a) {
	return {$: 'MyDocId', a: a};
};
var $author$project$Menkayonta$identifierToString = function (ident) {
	switch (ident.$) {
		case 'MyDocId':
			var ident_ = ident.a;
			return $author$project$Menkayonta$docIdToString(ident_);
		case 'MyTagId':
			var ident_ = ident.a;
			return $author$project$Menkayonta$tagIdToString(ident_);
		case 'MyPropertyId':
			var ident_ = ident.a;
			return $author$project$Menkayonta$propertyIdToString(ident_);
		case 'MyDescriptionId':
			var ident_ = ident.a;
			return $author$project$Menkayonta$descriptionIdToString(ident_);
		case 'MyModificationId':
			var ident_ = ident.a;
			return $author$project$Menkayonta$modificationIdToString(ident_);
		default:
			var ident_ = ident.a;
			return $author$project$Menkayonta$utilityIdToString(ident_);
	}
};
var $author$project$Converter$personIdString = function (str) {
	return $author$project$Menkayonta$identifierToString(
		$author$project$Menkayonta$MyDocId(
			$author$project$Menkayonta$PersonId(str)));
};
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $author$project$Converter$constructPerson = function (pdata) {
	var last_name = $elm$core$String$trim(pdata.last);
	var first_name = $elm$core$String$trim(pdata.first);
	var name = A2(
		$elm$core$String$join,
		' ',
		_List_fromArray(
			[first_name, last_name]));
	var email = first_name + ('.' + (last_name + '@example.com'));
	var newperson = {
		id: email,
		names: A2($elm$core$Dict$singleton, pdata.id, name),
		rev: $elm$core$Maybe$Nothing,
		version: 1
	};
	var stringid = $author$project$Converter$personIdString(email);
	return {person: newperson, stringId: stringid};
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $author$project$Converter$perhapsMakePerson = F2(
	function (dperson, model) {
		var _v0 = A2($elm$core$Dict$get, dperson.id, model.people);
		if (_v0.$ === 'Just') {
			var person = _v0.a;
			return {
				person: person,
				stringId: $author$project$Converter$personIdString(person.id)
			};
		} else {
			return $author$project$Converter$constructPerson(
				{first: dperson.first_name, id: dperson.id, last: dperson.last_name});
		}
	});
var $elm_community$maybe_extra$Maybe$Extra$unwrap = F3(
	function (_default, f, m) {
		if (m.$ === 'Nothing') {
			return _default;
		} else {
			var a = m.a;
			return f(a);
		}
	});
var $author$project$Converter$when = F2(
	function (input, _default) {
		return A3(
			$elm_community$maybe_extra$Maybe$Extra$unwrap,
			_default,
			function (x) {
				return A3(
					$elm$core$Dict$insert,
					$author$project$Menkayonta$identifierToString(x.id),
					x.val,
					_default);
			},
			input);
	});
var $author$project$Converter$elicitorStage = function (_v0) {
	var docid = _v0.docid;
	var curr = _v0.curr;
	var elicitor = _v0.elicitor;
	var model = _v0.model;
	var p = A2($author$project$Converter$perhapsMakePerson, elicitor, model);
	var people = A3($elm$core$Dict$insert, elicitor.id, p.person, model.people);
	var modDate_ = A4($author$project$Converter$modDate, 'elicitation', curr.date_elicited, docid, p.person.id);
	var elicitationMethod = A3(
		$author$project$Converter$maybeConstProp,
		'elicitation method',
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $.name;
			},
			curr.elicitation_method),
		docid);
	var comment_ = A3($author$project$Converter$constNonBlankDesc, 'comment', curr.comments, docid);
	var newto = A2(
		$author$project$Converter$when,
		elicitationMethod,
		A2(
			$author$project$Converter$when,
			modDate_,
			A2(
				$author$project$Converter$when,
				comment_,
				A3(
					$elm$core$Dict$insert,
					p.stringId,
					$author$project$Menkayonta$MyPerson(p.person),
					model.to))));
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{people: people, stage: $author$project$Converter$EntererS, to: newto}),
		$author$project$Converter$next('Completed ElicitorS'));
};
var $author$project$DativeTypes$dativeNamedEncoder = function (named) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(named.id)),
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(named.name))
			]));
};
var $author$project$DativeTypes$dativePersonEncoder = function (person) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(person.id)),
				_Utils_Tuple2(
				'first_name',
				$elm$json$Json$Encode$string(person.first_name)),
				_Utils_Tuple2(
				'last_name',
				$elm$json$Json$Encode$string(person.last_name)),
				_Utils_Tuple2(
				'role',
				$elm$json$Json$Encode$string(person.role))
			]));
};
var $author$project$DativeTypes$dativeSpeakerEncoder = function (speaker) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(speaker.id)),
				_Utils_Tuple2(
				'first_name',
				$elm$json$Json$Encode$string(speaker.first_name)),
				_Utils_Tuple2(
				'last_name',
				$elm$json$Json$Encode$string(speaker.last_name)),
				_Utils_Tuple2(
				'dialect',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, speaker.dialect))
			]));
};
var $author$project$DativeTypes$dativeTranslationEncoder = function (tr) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'id',
				$elm$json$Json$Encode$int(tr.id)),
				_Utils_Tuple2(
				'transcription',
				$elm$json$Json$Encode$string(tr.transcription)),
				_Utils_Tuple2(
				'grammaticality',
				$elm$json$Json$Encode$string(tr.grammaticality))
			]));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMillis = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			1000,
			$elm$time$Time$posixToMillis(time));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$Apr = {$: 'Apr'};
var $elm$time$Time$Aug = {$: 'Aug'};
var $elm$time$Time$Dec = {$: 'Dec'};
var $elm$time$Time$Feb = {$: 'Feb'};
var $elm$time$Time$Jan = {$: 'Jan'};
var $elm$time$Time$Jul = {$: 'Jul'};
var $elm$time$Time$Jun = {$: 'Jun'};
var $elm$time$Time$Mar = {$: 'Mar'};
var $elm$time$Time$May = {$: 'May'};
var $elm$time$Time$Nov = {$: 'Nov'};
var $elm$time$Time$Oct = {$: 'Oct'};
var $elm$time$Time$Sep = {$: 'Sep'};
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_v0) {
			case 1:
				return $elm$time$Time$Jan;
			case 2:
				return $elm$time$Time$Feb;
			case 3:
				return $elm$time$Time$Mar;
			case 4:
				return $elm$time$Time$Apr;
			case 5:
				return $elm$time$Time$May;
			case 6:
				return $elm$time$Time$Jun;
			case 7:
				return $elm$time$Time$Jul;
			case 8:
				return $elm$time$Time$Aug;
			case 9:
				return $elm$time$Time$Sep;
			case 10:
				return $elm$time$Time$Oct;
			case 11:
				return $elm$time$Time$Nov;
			default:
				return $elm$time$Time$Dec;
		}
	});
var $rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString = F2(
	function (digits, time) {
		return A3(
			$elm$core$String$padLeft,
			digits,
			_Utils_chr('0'),
			$elm$core$String$fromInt(time));
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime = function (time) {
	return A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		4,
		A2($elm$time$Time$toYear, $elm$time$Time$utc, time)) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		$rtfeldman$elm_iso8601_date_strings$Iso8601$fromMonth(
			A2($elm$time$Time$toMonth, $elm$time$Time$utc, time))) + ('-' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toDay, $elm$time$Time$utc, time)) + ('T' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toHour, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toMinute, $elm$time$Time$utc, time)) + (':' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		2,
		A2($elm$time$Time$toSecond, $elm$time$Time$utc, time)) + ('.' + (A2(
		$rtfeldman$elm_iso8601_date_strings$Iso8601$toPaddedString,
		3,
		A2($elm$time$Time$toMillis, $elm$time$Time$utc, time)) + 'Z'))))))))))));
};
var $rtfeldman$elm_iso8601_date_strings$Iso8601$encode = A2($elm$core$Basics$composeR, $rtfeldman$elm_iso8601_date_strings$Iso8601$fromTime, $elm$json$Json$Encode$string);
var $author$project$DativeTypes$encoder = function (df) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'_id',
				$elm$json$Json$Encode$string(
					'interlinear/' + $TSFoster$elm_uuid$UUID$toString(df.uuid))),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$int(1)),
				_Utils_Tuple2(
				'oldid',
				$elm$json$Json$Encode$int(df.id)),
				_Utils_Tuple2(
				'transcription',
				$elm$json$Json$Encode$string(df.transcription)),
				_Utils_Tuple2(
				'phonetic_transcription',
				$elm$json$Json$Encode$string(df.phonetic_transcription)),
				_Utils_Tuple2(
				'narrow_phonetic_transcription',
				$elm$json$Json$Encode$string(df.narrow_phonetic_transcription)),
				_Utils_Tuple2(
				'morpheme_break',
				$elm$json$Json$Encode$string(df.morpheme_break)),
				_Utils_Tuple2(
				'morpheme_gloss',
				$elm$json$Json$Encode$string(df.morpheme_gloss)),
				_Utils_Tuple2(
				'comments',
				$elm$json$Json$Encode$string(df.comments)),
				_Utils_Tuple2(
				'speaker_comments',
				$elm$json$Json$Encode$string(df.speaker_comments)),
				_Utils_Tuple2(
				'grammaticality',
				$elm$json$Json$Encode$string(df.grammaticality)),
				_Utils_Tuple2(
				'date_elicited',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $rtfeldman$elm_iso8601_date_strings$Iso8601$encode, df.date_elicited)),
				_Utils_Tuple2(
				'datetime_entered',
				$rtfeldman$elm_iso8601_date_strings$Iso8601$encode(df.datetime_entered)),
				_Utils_Tuple2(
				'datetime_modified',
				$rtfeldman$elm_iso8601_date_strings$Iso8601$encode(df.datetime_modified)),
				_Utils_Tuple2(
				'syntactic_category_string',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, df.syntactic_category_string)),
				_Utils_Tuple2(
				'morpheme_break_ids',
				A2(
					$elm_community$json_extra$Json$Encode$Extra$maybe,
					$elm$json$Json$Encode$list($author$project$DativeTypes$dativeTokenEncoder),
					df.morpheme_break_ids)),
				_Utils_Tuple2(
				'morpheme_gloss_ids',
				A2(
					$elm_community$json_extra$Json$Encode$Extra$maybe,
					$elm$json$Json$Encode$list($author$project$DativeTypes$dativeTokenEncoder),
					df.morpheme_gloss_ids)),
				_Utils_Tuple2(
				'break_gloss_category',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, df.break_gloss_category)),
				_Utils_Tuple2(
				'syntax',
				$elm$json$Json$Encode$string(df.syntax)),
				_Utils_Tuple2(
				'semantics',
				$elm$json$Json$Encode$string(df.semantics)),
				_Utils_Tuple2(
				'status',
				$elm$json$Json$Encode$string(df.status)),
				_Utils_Tuple2(
				'elicitor',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativePersonEncoder, df.elicitor)),
				_Utils_Tuple2(
				'enterer',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativePersonEncoder, df.enterer)),
				_Utils_Tuple2(
				'modifier',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativePersonEncoder, df.modifier)),
				_Utils_Tuple2(
				'verifier',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativePersonEncoder, df.verifier)),
				_Utils_Tuple2(
				'speaker',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativeSpeakerEncoder, df.speaker)),
				_Utils_Tuple2(
				'elicitation_method',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativeNamedEncoder, df.elicitation_method)),
				_Utils_Tuple2(
				'syntactic_category',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $author$project$DativeTypes$dativeNamedEncoder, df.syntactic_category)),
				_Utils_Tuple2(
				'source',
				A2($elm_community$json_extra$Json$Encode$Extra$maybe, $elm$json$Json$Encode$string, df.source)),
				_Utils_Tuple2(
				'translations',
				A2($elm$json$Json$Encode$list, $author$project$DativeTypes$dativeTranslationEncoder, df.translations)),
				_Utils_Tuple2(
				'tags',
				A2($elm$json$Json$Encode$list, $author$project$DativeTypes$dativeNamedEncoder, df.tags)),
				_Utils_Tuple2(
				'files',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, df.files))
			]));
};
var $author$project$Converter$entererStage = function (_v0) {
	var docid = _v0.docid;
	var curr = _v0.curr;
	var enterer = _v0.enterer;
	var model = _v0.model;
	var p = A2($author$project$Converter$perhapsMakePerson, enterer, model);
	var people = A3($elm$core$Dict$insert, enterer.id, p.person, model.people);
	var modDate_ = A4(
		$author$project$Converter$modDate,
		'entered',
		$elm$core$Maybe$Just(curr.datetime_entered),
		docid,
		p.person.id);
	var newto = A2(
		$author$project$Converter$when,
		modDate_,
		A3(
			$elm$core$Dict$insert,
			p.stringId,
			$author$project$Menkayonta$MyPerson(p.person),
			model.to));
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{people: people, stage: $author$project$Converter$ModifierS, to: newto}),
		$author$project$Converter$next('Completed EntererS'));
};
var $author$project$Menkayonta$MyInterlinear = function (a) {
	return {$: 'MyInterlinear', a: a};
};
var $author$project$Converter$constNonBlankProp = F3(
	function (kind, str, docid) {
		return $elm$core$String$isEmpty(str) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
			A3($author$project$Converter$constructProperty, kind, str, docid));
	});
var $author$project$Menkayonta$MyTag = function (a) {
	return {$: 'MyTag', a: a};
};
var $author$project$Menkayonta$MyTagId = function (a) {
	return {$: 'MyTagId', a: a};
};
var $author$project$Converter$constructTag = F2(
	function (kind, docid) {
		return function (x) {
			return {
				id: $author$project$Menkayonta$MyTagId(x.id),
				val: $author$project$Menkayonta$MyTag(x)
			};
		}(
			{
				id: {docid: docid, fragment: $elm$core$Maybe$Nothing, kind: kind},
				rev: $elm$core$Maybe$Nothing,
				version: 1
			});
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $author$project$Converter$tail = function (list) {
	if (!list.b) {
		return _List_Nil;
	} else {
		var tl = list.b;
		return tl;
	}
};
var $author$project$Converter$interlinearStage = F2(
	function (curr, model) {
		var interlinear = {
			ann: {breaks: curr.morpheme_break, glosses: curr.morpheme_gloss, judgment: curr.grammaticality, phonemic: curr.phonetic_transcription},
			id: curr.uuid,
			rev: $elm$core$Maybe$Nothing,
			text: curr.transcription,
			translations: $elm$core$Dict$fromList(
				A2(
					$elm$core$List$map,
					function (t) {
						return _Utils_Tuple2(
							t.id,
							{judgment: t.grammaticality, translation: t.transcription});
					},
					curr.translations)),
			version: 1
		};
		var interlineardoc = $author$project$Menkayonta$MyInterlinear(interlinear);
		var docid = $author$project$Menkayonta$InterlinearId(curr.uuid);
		var nptrans = A3($author$project$Converter$constNonBlankDesc, 'narrow phonetic transription', curr.narrow_phonetic_transcription, docid);
		var semantics = A3($author$project$Converter$constNonBlankProp, 'semantics', curr.semantics, docid);
		var source = A3($author$project$Converter$maybeConstProp, 'source', curr.source, docid);
		var status = A3($author$project$Converter$constNonBlankProp, 'status', curr.status, docid);
		var stringid = $author$project$Menkayonta$identifierToString(
			$author$project$Menkayonta$MyDocId(docid));
		var syntactic_category = A3(
			$author$project$Converter$maybeConstProp,
			'syntactic category',
			A2(
				$elm$core$Maybe$map,
				function ($) {
					return $.name;
				},
				curr.syntactic_category),
			docid);
		var syntax = A3($author$project$Converter$constNonBlankProp, 'syntax', curr.syntax, docid);
		var tags = A2(
			$elm$core$List$map,
			function (t) {
				return A2($author$project$Converter$constructTag, t.name, docid);
			},
			curr.tags);
		var newto = A2(
			$author$project$Converter$when,
			source,
			A2(
				$author$project$Converter$when,
				syntactic_category,
				A2(
					$author$project$Converter$when,
					status,
					A2(
						$author$project$Converter$when,
						semantics,
						A2(
							$author$project$Converter$when,
							syntax,
							A2(
								$author$project$Converter$when,
								nptrans,
								function (indict) {
									return A3(
										$elm$core$List$foldl,
										F2(
											function (t, indict_) {
												return A3(
													$elm$core$Dict$insert,
													$author$project$Menkayonta$identifierToString(t.id),
													t.val,
													indict_);
											}),
										indict,
										tags);
								}(
									A3($elm$core$Dict$insert, stringid, interlineardoc, model.to))))))));
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{
					from: $author$project$Converter$tail(model.from),
					stage: $author$project$Converter$OriginalS,
					to: newto
				}),
			$author$project$Converter$next('Completed InterlinearS'));
	});
var $author$project$Converter$modifierStage = function (_v0) {
	var docid = _v0.docid;
	var curr = _v0.curr;
	var modifier = _v0.modifier;
	var model = _v0.model;
	var p = A2($author$project$Converter$perhapsMakePerson, modifier, model);
	var people = A3($elm$core$Dict$insert, modifier.id, p.person, model.people);
	var modDate_ = A4(
		$author$project$Converter$modDate,
		'updated',
		$elm$core$Maybe$Just(curr.datetime_modified),
		docid,
		p.person.id);
	var newto = A2(
		$author$project$Converter$when,
		modDate_,
		A3(
			$elm$core$Dict$insert,
			p.stringId,
			$author$project$Menkayonta$MyPerson(p.person),
			model.to));
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{people: people, stage: $author$project$Converter$VerifierS, to: newto}),
		$author$project$Converter$next('Completed ModiferS'));
};
var $author$project$Converter$speakerStage = function (_v0) {
	var docid = _v0.docid;
	var curr = _v0.curr;
	var speaker = _v0.speaker;
	var model = _v0.model;
	var speakerComment = A3($author$project$Converter$constNonBlankDesc, 'speaker comment', curr.speaker_comments, docid);
	var p = function () {
		var _v1 = A2($elm$core$Dict$get, -speaker.id, model.people);
		if (_v1.$ === 'Just') {
			var person = _v1.a;
			return {
				person: person,
				stringId: $author$project$Converter$personIdString(person.id)
			};
		} else {
			return $author$project$Converter$constructPerson(
				{first: speaker.first_name, id: speaker.id, last: speaker.last_name});
		}
	}();
	var people = A3($elm$core$Dict$insert, -speaker.id, p.person, model.people);
	var personid = $author$project$Menkayonta$PersonId(p.person.id);
	var speakerDialect = A3($author$project$Converter$maybeConstProp, 'dialect', speaker.dialect, personid);
	var docDialect = A3($author$project$Converter$maybeConstProp, 'dialect', speaker.dialect, docid);
	var newto = A2(
		$author$project$Converter$when,
		speakerComment,
		A2(
			$author$project$Converter$when,
			docDialect,
			A2(
				$author$project$Converter$when,
				speakerDialect,
				A3(
					$elm$core$Dict$insert,
					p.stringId,
					$author$project$Menkayonta$MyPerson(p.person),
					model.to))));
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{people: people, stage: $author$project$Converter$ElicitorS, to: newto}),
		$author$project$Converter$next('Completed SpeakerS'));
};
var $author$project$Menkayonta$MyUtility = function (a) {
	return {$: 'MyUtility', a: a};
};
var $author$project$Menkayonta$MyUtilityId = function (a) {
	return {$: 'MyUtilityId', a: a};
};
var $author$project$Converter$utilityStage = function (_v0) {
	var docid = _v0.docid;
	var utilityVal = _v0.utilityVal;
	var model = _v0.model;
	var utility = {
		id: {docid: docid, fragment: $elm$core$Maybe$Nothing, kind: 'dative'},
		rev: $elm$core$Maybe$Nothing,
		value: utilityVal,
		version: 1
	};
	var stringid = $author$project$Menkayonta$identifierToString(
		$author$project$Menkayonta$MyUtilityId(utility.id));
	var newto = A3(
		$elm$core$Dict$insert,
		stringid,
		$author$project$Menkayonta$MyUtility(utility),
		model.to);
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{stage: $author$project$Converter$InterlinearS, to: newto}),
		$author$project$Converter$next('Completed UtilityS'));
};
var $author$project$Converter$verifierStage = function (_v0) {
	var docid = _v0.docid;
	var curr = _v0.curr;
	var verifier = _v0.verifier;
	var model = _v0.model;
	var p = A2($author$project$Converter$perhapsMakePerson, verifier, model);
	var people = A3($elm$core$Dict$insert, verifier.id, p.person, model.people);
	var modDate_ = A4(
		$author$project$Converter$modDate,
		'verified',
		$elm$core$Maybe$Just(curr.datetime_modified),
		docid,
		p.person.id);
	var newto = A2(
		$author$project$Converter$when,
		modDate_,
		A3(
			$elm$core$Dict$insert,
			p.stringId,
			$author$project$Menkayonta$MyPerson(p.person),
			model.to));
	return _Utils_Tuple2(
		_Utils_update(
			model,
			{people: people, stage: $author$project$Converter$UtilityS, to: newto}),
		$author$project$Converter$next('Completed VerifierS'));
};
var $author$project$Converter$resolveStage = F2(
	function (curr, model) {
		var docid = $author$project$Menkayonta$InterlinearId(curr.uuid);
		var _v0 = model.stage;
		switch (_v0.$) {
			case 'OriginalS':
				var constm = {
					docid: docid,
					json: $author$project$DativeTypes$encoder(curr),
					kind: 'importsource',
					pid: model.me,
					time: model.time
				};
				var m = $author$project$Converter$constructModifier(constm);
				var newto = A3(
					$elm$core$Dict$insert,
					$author$project$Menkayonta$identifierToString(m.id),
					m.val,
					model.to);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{stage: $author$project$Converter$SpeakerS, to: newto}),
					$author$project$Converter$next('Completed OriginalS'));
			case 'SpeakerS':
				var _v1 = curr.speaker;
				if (_v1.$ === 'Just') {
					var speaker = _v1.a;
					return $author$project$Converter$speakerStage(
						{curr: curr, docid: docid, model: model, speaker: speaker});
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{stage: $author$project$Converter$ElicitorS}),
						$author$project$Converter$next('Skip SpeakerS'));
				}
			case 'ElicitorS':
				var _v2 = curr.elicitor;
				if (_v2.$ === 'Just') {
					var elicitor = _v2.a;
					return $author$project$Converter$elicitorStage(
						{curr: curr, docid: docid, elicitor: elicitor, model: model});
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{stage: $author$project$Converter$EntererS}),
						$author$project$Converter$next('Skip ElicitorS'));
				}
			case 'EntererS':
				var _v3 = curr.enterer;
				if (_v3.$ === 'Just') {
					var enterer = _v3.a;
					return $author$project$Converter$entererStage(
						{curr: curr, docid: docid, enterer: enterer, model: model});
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{stage: $author$project$Converter$ModifierS}),
						$author$project$Converter$next('Skip EntererS'));
				}
			case 'ModifierS':
				var _v4 = curr.modifier;
				if (_v4.$ === 'Just') {
					var modifier = _v4.a;
					return $author$project$Converter$modifierStage(
						{curr: curr, docid: docid, model: model, modifier: modifier});
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{stage: $author$project$Converter$VerifierS}),
						$author$project$Converter$next('Skip ModifierS'));
				}
			case 'VerifierS':
				var _v5 = curr.verifier;
				if (_v5.$ === 'Just') {
					var verifier = _v5.a;
					return $author$project$Converter$verifierStage(
						{curr: curr, docid: docid, model: model, verifier: verifier});
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{stage: $author$project$Converter$UtilityS}),
						$author$project$Converter$next('Skip VerifierS'));
				}
			case 'UtilityS':
				var utilityVal = {break_gloss_category: curr.break_gloss_category, morpheme_break_ids: curr.morpheme_break_ids, morpheme_gloss_ids: curr.morpheme_gloss_ids, syntactic_category_string: curr.syntactic_category_string, version: 1};
				var empty = A5($author$project$Converter$DativeUtilityValue, 1, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
				return _Utils_eq(utilityVal, empty) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{stage: $author$project$Converter$InterlinearS}),
					$author$project$Converter$next('Skip UtilityS')) : $author$project$Converter$utilityStage(
					{
						docid: docid,
						model: model,
						utilityVal: $author$project$Converter$dativeUtilEncoder(utilityVal)
					});
			default:
				return A2($author$project$Converter$interlinearStage, curr, model);
		}
	});
var $author$project$Converter$sendBulkDocs = _Platform_outgoingPort('sendBulkDocs', $elm$core$Basics$identity);
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Converter$update = F2(
	function (msg, model) {
		if (msg.$ === 'ReceivedDativeForms') {
			var job = msg.a;
			var _v1 = A2($elm$json$Json$Decode$decodeValue, $author$project$Converter$jobDecoder, job);
			if (_v1.$ === 'Err') {
				var e = _v1.a;
				var error = $elm$json$Json$Decode$errorToString(e);
				return _Utils_Tuple2(
					model,
					$author$project$Converter$reportError(error));
			} else {
				var j = _v1.a;
				var _v2 = A2(
					$elm$json$Json$Decode$decodeValue,
					$elm$json$Json$Decode$list($author$project$DativeTypes$decoder),
					j.payload);
				if (_v2.$ === 'Err') {
					var e_ = _v2.a;
					var error = $elm$json$Json$Decode$errorToString(e_);
					return _Utils_Tuple2(
						model,
						$author$project$Converter$reportError(error));
				} else {
					var dts = _v2.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{from: dts}),
						$author$project$Converter$next('Received Dative Forms'));
				}
			}
		} else {
			var _v3 = model.from;
			if (!_v3.b) {
				var job = {
					payload: A2(
						$elm$json$Json$Encode$list,
						$author$project$Menkayonta$encoder,
						$elm$core$Dict$values(model.to)),
					project: model.project
				};
				var jval = $author$project$Converter$jobEncoder(job);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{stage: $author$project$Converter$OriginalS}),
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Converter$sendBulkDocs(jval),
								$author$project$Converter$reportInfo('Completed Processing')
							])));
			} else {
				var curr = _v3.a;
				return A2($author$project$Converter$resolveStage, curr, model);
			}
		}
	});
var $elm$core$Platform$worker = _Platform_worker;
var $author$project$Converter$main = $elm$core$Platform$worker(
	{init: $author$project$Converter$init, subscriptions: $author$project$Converter$subscriptions, update: $author$project$Converter$update});
_Platform_export({'Converter':{'init':$author$project$Converter$main(
	A2(
		$elm$json$Json$Decode$andThen,
		function (time) {
			return A2(
				$elm$json$Json$Decode$andThen,
				function (seed4) {
					return A2(
						$elm$json$Json$Decode$andThen,
						function (seed3) {
							return A2(
								$elm$json$Json$Decode$andThen,
								function (seed2) {
									return A2(
										$elm$json$Json$Decode$andThen,
										function (seed1) {
											return A2(
												$elm$json$Json$Decode$andThen,
												function (project) {
													return A2(
														$elm$json$Json$Decode$andThen,
														function (me) {
															return $elm$json$Json$Decode$succeed(
																{me: me, project: project, seed1: seed1, seed2: seed2, seed3: seed3, seed4: seed4, time: time});
														},
														A2($elm$json$Json$Decode$field, 'me', $elm$json$Json$Decode$string));
												},
												A2($elm$json$Json$Decode$field, 'project', $elm$json$Json$Decode$string));
										},
										A2($elm$json$Json$Decode$field, 'seed1', $elm$json$Json$Decode$int));
								},
								A2($elm$json$Json$Decode$field, 'seed2', $elm$json$Json$Decode$int));
						},
						A2($elm$json$Json$Decode$field, 'seed3', $elm$json$Json$Decode$int));
				},
				A2($elm$json$Json$Decode$field, 'seed4', $elm$json$Json$Decode$int));
		},
		A2($elm$json$Json$Decode$field, 'time', $elm$json$Json$Decode$int)))(0)}});}(this));