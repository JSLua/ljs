testCases = [

    {
        name: "Standard expression priority",
        a: 1,
        run() {
            if (eval("1+1+1==1") !== false)
                throw '#2: eval("1+1+1==1") !== false';

            let x="5+1|0===0";
            if (eval(x) !== 7)
                throw '#1: eval(x) !== 7';
            if (eval(`2*${x}>-1`) !== 11)
                throw '#2: eval(`2*${x}>-1`) !== 11';

            let b = 2;
            this.c = 3;
            if (this.a + b * this.c != 7)
                throw "this.a + b * this.c != 7";
            if ((this.a + b) * this.c != 9)
                throw "(this.a + b) * this.c != 9";
            if (this.a + b ** this.c != 9)
                throw "this.a + b ** this.c != 9";
        }
    },

    {
        name: "Logical expression short circuit",
        val: "Test val",
        num_1: 1,
        run() {
            let a = this.num_1;
            if ((a == 1 && this.val || "zzz") != "Test val")
                throw "this.val not yielded";
            if ((!"zzz" || a == 1 && this.val) != "Test val")
                throw "this.val not yielded";
            if ((!"zzz" || this.val && a == 1) != true)
                throw "true (a == 1) not yielded";
            if ((!"zzz" || a == 2) && (a == 1) != false)
                throw "false (a == 2) not yielded";

            let fun = function (a, b, c) { return !(a && b) || c; };
            if (fun(null, "bb", "cc") !== true ||
                fun("aa", "bb", "cc") !== "cc")
                throw "Incorrect logical not behaviour";

            let num = 5;
            if (-!(num & 2) !== -1)
                throw "Incorrect logical not coercion";
        }
    },

    {
        name: "Left Val Reference",
        tab: { a: 1 },
        o: { n: 1 },
        d: 1,
        run() {
            this.tab.a += 2;
            if (this.tab.a != 3)
                throw "Unexpected this.tab.a != 3";

            let b = 6;
            if (b-- != 6)
                throw "Unexpected b-- != 6";
            if (b != 5)
                throw "Unexpected b != 5";

            let c = 7;
            with (this.tab) {
                c += a;
                a += 2;
            }
            if (c != 10 || this.tab.a != 5)
                throw "Unexpected c != 10 || a != 5";

            if (this.o.n++ !== 1 || !(this.o.n & 2))
                throw "Unexpected postfix increment result";
            if (this.o[String.fromCharCode(110)]++ !== 2 || this.o.n !== 3)
                throw "Unexpected postfix increment result";

            this.d += 2;
            let d;
            if (d != null)
                throw "d is not initialized";
        }
    },

    {
        name: "IfStatement and else binding",
        a: 1,
        b: 2,
        run() {
            let passThen = false, passElse = false;
            if (this.a != 1 || this.b == 2) {
                passThen = true;
            } else {
                passElse = true;
            }
            if (passThen == false || passElse == true)
                throw "Unexpected passThen == false || passElse == true";

            passThen = false, passElse = false;
            if (this.a == 1 && this.b == 1) {
                passThen = true;
            } else {
                passElse = true;
            }
            if (passThen == true || passElse == false)
                throw "Unexpected passThen == true || passElse == false";

            //CHECK# 1
            if(true)
                if (false)
                    throw `#1.1: At embedded "if/else" constructions engine must select right branches`;
                else
                    ;

            //CHECK# 2
            if(true)
                if (true)
                    ;
                else
                    throw `#2.1: At embedded "if/else" constructions engine must select right branches`;

            //CHECK# 3
            if(false)
                if (true)
                    throw `#3.1: At embedded "if/else" constructions engine must select right branches`;
                else
                    throw `#3.2: At embedded "if/else" constructions engine must select right branches`;

            //CHECK# 4
            if(false)
                if (true)
                    throw `#4.1: At embedded "if/else" constructions engine must select right branches`;
                else
                    throw `#4.2: At embedded "if/else" constructions engine must select right branches`;
        }
    },

    {
        name: "Compile-time bitwise arith",
        run() {
            const assert = b => { if (!b) throw "assertion failed"; };
            assert(eval("(0xFFFFFFFF + 1) | 0") === 0);
            assert(eval("0x100000000 | 0") === 0);
            assert(eval("0xFFFFFFFF | 0") === -1);
            assert(eval("(2 ** 40) | 0") === 0);
            assert(eval("((2 ** 40) + 123) | 0") === 123);

            assert(eval("-1 >> 1") === -1);
            assert(eval("-1 >>> 1") === 2147483647);
            assert(eval("0x80000000 >> 0") === -2147483648);
            assert(eval("1 << 33") === 2);   // 33 % 32 == 1
            assert(eval("1 << 64") === 1);
            assert(eval("1 << -1") === -2147483648);  // -1 & 31 == 31
            assert(eval("1 << -32") === 1);

            assert(eval("0x7FFFFFFF | 0") === 2147483647);
            assert(eval("0x80000000 | 0") === -2147483648);
            assert(eval("(0x7FFFFFFF + 1) | 0") === -2147483648);
            assert(eval("~-1") === 0);
            assert(eval("~-2147483648") === 2147483647);
        }
    },

    {
        name: "Iteration Statements",
        run() {
            let count = 0;
            for (let obj of ["Apple", "Banana", "Cherry"])
                count++;
            if (count != 3)
                throw "Incorrect iteration count";

            count = 0;
            for (let i = 0; i < 10; i++)
                count += i;
            if (count != 45)
                throw `Incorrect result of sum(0..9): ${count}`;

            let i;
            count = 0;
            for (i = 0; ; i++) {
                if (i == 5) continue;
                if (i == 10) break;
                count += i;
            }
            if (count != 40)
                throw `Incorrect result of sum(0..9): ${count}`;

            let __in__do;
            do __in__do = 1; while (false)
            if (__in__do !== 1)
                throw 'the inner statement of a do-loop should be evaluated before the expression';

            i = 0, count = 0;
            do {
                count += i;
            } while(i++ < 10)
            if (count != 55)
                throw `Incorrect result of sum(0..10): ${count}`;

            let o = { val: 1, next: { val: 2, next: { val: 3 } } };
            count = 0;
            for (; o; o = o.next)
                count += o.val;
            if (count != 6)
                throw `Incorrect result of sum(list): ${count}`;

            let in_for;
            try {
                for((function(){throw "NoInExpression";})();
                    (function(){throw "FirstExpression";})();
                    (function(){throw "SecondExpression";})())
                    in_for = "reached";
                throw `(function(){throw "NoInExpression";})() should throw exception`;
            } catch (e) {
                if (!e.endsWith("NoInExpression"))
                    throw "ExpressionNoIn not evaluated first";
            }
            if (in_for != null)
                throw 'in_for should not be touched';
        }
    },

    {
        name: "Linked-list Iterate and Sum",
        run() {
            let sum = 0;
            let o = { val: 1, next: { val: 2, next: { val: 3 } } };
            for (; o; o = o.next)
                sum += o.val;

            function *iterateList(list) {
                for (; list; list = list.next)
                    yield list.val;
            }

            o = { val: 4, next: { val: 5, next: { val: 6 } } };
            for (let val of iterateList(o))
                sum += val;
            if (sum != 21)
                throw `Incorrect result of sum(list): ${sum}`;
        }
    },

    {
        name: "Function default and rest parameters",
        run() {
            let f = function(a = 111, b = "Default") {
                if (a !== 222) throw "a is incorrectly initialized";
                if (b !== "Default") throw "e is not initialized to Default";
            };
            f(222);

            let arr = [
                (function(...args) { return args.length; })(1,2,3,4,5),
                (function(a, ...args) { return args.length; })(1,2,3,4,5),
                (function(a, b, ...args) { return args.length; })(1,2,3,4,5),
                (function(a, b, c, ...args) { return args.length; })(1,2,3,4,5),
                (function(a, b, c, d, ...args) { return args.length; })(1,2,3,4,5),
                (function(a, b, c, d, e, ...args) { return args.length; })(1,2,3,4,5),
            ];
            let sum = 0;
            for (let n of arr)
                sum += n;
            if (sum != 15)
                throw `Sum of rest parameter count ${sum} is unexpected`;

            let obj = {
                str: "Bear likes",
                test_rest(a, ...c) {
                    with (this)
                        str += ` ${a}`;
                    this.tab = [ "Alpine", ...c ];
                }
            };
            obj.test_rest("Apple", "Banana", "Cherry");
            if (obj.str != "Bear likes Apple")
                throw "obj.str != \"Bear likes Apple\"";
            let all = "";
            for (let k of obj.tab)
                all += `${k}\n`;
            if (all != "Alpine\nBanana\nCherry\n")
                throw "List is not correct";
        }
    },

    {
        name: "Closure and Arrow this inheritance",
        run() {
            let closure;
            (function() {
                let a = 1;
                while (!closure) {
                    let b = 2;
                    {
                        let c = 3;
                        closure = function() { return a + b + c; };
                    }
                }
            })();
            if (closure() !== 6)
                throw "Unexpected closure return value";

            var calls = 0;
            var usurper = { fun: value => {
                calls++;
                if (this === usurper)
                    throw "Arrow incorrectly not inherited this";
            } };
            usurper.fun();
            if (calls != 1)
                throw "Upvalue `calls` unexpected not changed";
        }
    },

    {
        name: "Labelled loop and continue with label",
        run() {
            let object = {p1: 1, p2: 1};
            let result = 0;
            lbl: for (let i in object) { result += object[i]; break lbl; }
            if (!(result === 1))
                throw "'break lbl' should break execution of labelled iteration";

            let sum = 0, count = 0;
            outer: for (let j = 1; j < 3; j++) {
                for (let i = 1; i <= 7; i++) {
                    sum += i * j;
                    if (i > j * 3)
                        continue outer;
                }
                count++;
            }
            if (sum !== 66)
                throw `Incorrect result of sum (${sum} != 66)`;
            if (count !== 0)
                throw `Incorrect result of count (${count} != 0)`;
        }
    },

    {
        name: "SwitchStatement fallthrough behaviours",
        run() {
            const testSwitch1 = value => {
                var result = 0;
                switch(value) {
                    case 0: result += 2;
                    case 1: result += 4; break;
                    case 2: result += 8;
                    case 3: result += 16;
                    default:result += 32;
                }
                return result;
            };

            if (testSwitch1(0) !== 6)
                throw "Unexpected testSwitch1(0) result";
            if (testSwitch1(1) !== 4)
                throw "Unexpected testSwitch1(1) result";
            if (testSwitch1(2) !== 56)
                throw "Unexpected testSwitch1(2) result";
            if (testSwitch1(3) !== 48)
                throw "Unexpected testSwitch1(3) result";
            if (testSwitch1(void 0) !== 32)
                throw "Unexpected testSwitch1(void 0) result";
            if (testSwitch1('0') !== 32)
                throw "Unexpected testSwitch1('0') result";

            const testSwitch2 = value => {
                var result = 0;
                switch(value) {
                    case 0:
                        switch(value) {
                            case 0: result += 3; break;
                            default:result += 32;break;
                        }
                        result *= 2;
                        break;
                        result=3;
                    default:
                        result += 32;
                        break;
                }
                return result;
            };

            if (testSwitch2(0) !== 6)
                throw "Unexpected testSwitch2('0') result";
        }
    },

    {
        name: "Return value across WithStatement",
        run() {
            let f = function(o) {
                function innerf(o) {
                    with (o) {
                        return x + 1;
                    }
                }
                return innerf(o);
            };
            if (f({x: 42}) !== 43)
                throw `Returned value is not expected`;

            let noTouch = "untouched";
            const testFun = () => {
                with ({var_inner: "inner"}) return var_inner;
                noTouch = "touched";
                return "outer";
            };
            const retVal = testFun();
            if (retVal !== "inner")
                throw "return within WithStatement not effective";
            if (noTouch !== "untouched")
                throw "variable after is touched";
        }
    },

    {
        name: "TryStatement catch err object",
        run() {
            try {
                (() => { throw "error string"; })();
            }
            catch (err) {
                if (!err.endsWith("error string"))
                    throw "Returned message is not `error string`";
            }

            class SomeError {
                constructor(message) {
                    this.message = message;
                }

                getMessage() {
                    return this.message;
                }
            }

            try {
                (() => { throw new SomeError("error object"); })();
            }
            catch (err) {
                if (!(err instanceof SomeError))
                    throw "Returned value is not a SomeError";
                if (err.getMessage() !== "error object")
                    throw "err.getMessage() return value is not expected";
            }
        }
    },

    {
        name: "TryStatement with Finally behaviours",
        run() {
            let shouldTouch1 = false;
            const testFun1 = () => {
                try { return "inner"; }
                finally { shouldTouch1 = true; }
                return "outer";
            };
            if (testFun1() !== "inner")
                throw "return within TryStatement not effective";
            if (!shouldTouch1)
                throw "finally within TryStatement not executed";

            let shouldTouch2 = false;
            let shouldNotTouch2 = false;
            const testFun2 = () => {
                try { return "inner"; throw "no reach"; }
                catch (_) { shouldNotTouch2 = true; }
                finally { shouldTouch2 = true; }
                return "outer";
            };
            if (testFun2() !== "inner")
                throw "return within TryStatement not effective";
            if (shouldNotTouch2)
                throw "catch within TryStatement executed";
            if (!shouldTouch2)
                throw "finally within TryStatement not executed";

            let shouldNotTouch3 = false;
            const testFun3 = () => {
                try { return "inner"; throw "no reach"; }
                catch (_) { shouldNotTouch3 = true; }
                return "outer";
            };
            if (testFun3() !== "inner")
                throw "return within TryStatement not effective";
            if (shouldNotTouch3)
                throw "catch within TryStatement executed";
        }
    },

    {
        name: "Class inheritance and static fields",
        run() {
            let A = class {
                static static_v = "Apple";
                constructor() {
                    this.ins_v = "Banana";
                }
            };
            let B = class extends A {
                ins_v2;
                constructor() {
                    super();
                    this.ins_v2 = "Cherry";
                }
            };
            if (B.static_v !== "Apple")
                throw `Static field inherited from A is ${
                    B.static_v.toString() }`;

            let b = new B;
            if (b.constructor !== B)
                throw `Unexpected constructor for b - ${
                    b.constructor.toString() }`;
            if (b.ins_v !== "Banana")
                throw `Constructor of A not invoked, ins_v is ${b.ins_v}`;
            if (b.constructor.static_v !== "Apple")
                throw `Static field inherited from A is ${
                    B.static_v.toString() }`;

            class C extends B {}
            if (!(b instanceof A && b instanceof B))
                throw "instanceof check yielded false negative result";
            if (b instanceof C)
                throw "instanceof check yielded false positive result";
        }
    },

    {
        name: "Partial OptionalChain support",
        run() {
            const $ = 'x';
            const arr = [10, 11];
            const obj = {
                a: 'hello',
                $: 0,
                x: 43,
                b: {val: 13},
                arr: [11, 12]
            };
            const i = typeof arr[0] === "number" ? 0 : 1;
            if (11 !== arr?.[i + 1] ||
                'hello' !== obj?.a ||
                12 !== obj?.arr?.[i + 1] ||
                13 !== obj?.b?.val ||
                obj?.['$'] !== 0 ||
                obj?.[$] !== 43)
                throw "Unexpected OptionalChain behaviour";

            let x = 1;
            null?.[++x];
            if (x !== 1)
                throw "Unexpected OptionalChain short-circuiting";
        }
    },

    {
        name: "Builtin - Promise object",
        run() {
            if (!jsrt) return;  /* Not suitable for non-ljs runtimes */

            let returnValue;
            let { promise, resolve, reject } = Promise.withResolvers();
            promise.then(value => { returnValue = value; });
            resolve(55);
            jsrt.flushTask();
            if (returnValue !== 55)
                throw 'The promise should be fulfilled with the provided value.';

            returnValue = null;
            new Promise(_resolve => { resolve = _resolve; }).then(
                value => { returnValue = value; },
                () => { returnValue = 'The promise should not be rejected.'; });
            if (resolve(45) != null)  throw '"resolve" return value';
            jsrt.flushTask();
            if (returnValue !== 45)
                throw 'The promise should be fulfilled with the provided value.';

            let value = {};
            let thenable = new Promise(resolve => { resolve(value); });
            returnValue = null;
            new Promise(resolve => { returnValue = resolve(thenable); }).then(
                val => { returnValue = val; },
                () => { returnValue = 'The promise should not be rejected.'; });
            jsrt.flushTask();
            if (returnValue !== value)
                throw 'The promise should be fulfilled with the provided value.';

            let p1 = Promise.resolve(1),
                p2 = Promise.resolve(p1);
            if (p2 != p1)
                throw 'The value of p1 is expected to equal the value of p2';

            let rejected = false;
            thenable = new Promise(function() {});
            new Promise(function(resolve) {
                resolve();
                throw thenable;
            }).then(() => {}, () => { rejected = true; });
            jsrt.flushTask();
            if (rejected)
                throw 'The promise should not be rejected.';

            rejected = false;
            (async () =>
                await new Promise(_resolve => { resolve = _resolve; })
            )().catch(err => rejected = true);
            resolve(Promise.reject('test'));
            jsrt.flushTask();
            if (!rejected) throw 'Async function expects rejection';

            p1 = Promise.withResolvers(), p2 = Promise.withResolvers();
            let p3 = Promise.race([p1.promise, p2.promise]);
            p2.resolve("banana");
            p1.resolve("apple");
            p3.then(val=> { if (val !== "banana") throw "incorrect"; });
            jsrt.flushTask();
        }
    },

    {
        name: "Builtin - RegExp and matchAll",
        run() {
            const input = "apple banana carrot";
            const match = /ba([an]{2}){2}/.exec(input, 2);
            if (match.index !== 6 || match[0] !== 'banana' || match[1] !== 'na')
                throw 'Unexpected match result';
            if (match.toString() !== 'banana,na')
                throw 'Incorrect match.toString() result';
            if (match.input !== input)
                throw 'Match input object mismatch';

            let count = 0, prefix = '';
            for (const word of
                    "apple banana carrot".matchAll(/([a-z])[a-z]*/g)) {
                count += 1;
                prefix += word[1].toUpperCase();
            }
            if (count !== 3 || prefix !== 'ABC')
                throw 'Unexpected matchAll result';

            if ([..."apple banana carrot".matchAll('[a-z]+')].length
                    !== ["apple", "banana", "carrot"].length)
                throw 'Unexpected matchAll result';
        }
    }

];

let passed = 0, failed = 0;
for (let testCase of testCases) {
    console.log("Running test case \"%s\" ...", testCase.name);
    try {
        testCase.run();
        passed++;
    }
    catch (ex) {
        console.log("Error: %s", ex);
        failed++;
    }
}
console.log("%d / %d cases passed", passed, passed + failed);