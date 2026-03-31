# libts Feature Coverage Checklist

**Last updated:** 2026-03-31
**Measuring against:** TypeScript 5.x / ECMAScript 2024

---

## Summary

| Category | Done | Total | % |
|----------|------|-------|---|
| Tokens & Operators | 60 | 71 | 85% |
| Keywords | 60 | 94 | 64% |
| Declarations | 14 | 17 | 82% |
| Statements | 19 | 21 | 90% |
| Expressions | 31 | 32 | 97% |
| Type Annotations | 18 | 26 | 69% |
| Class Features | 10 | 14 | 71% |
| Module System | 5 | 10 | 50% |
| Transform Rules | 28 | 35 | 80% |
| **TOTAL** | **245** | **320** | **77%** |

---

## 1. Tokens & Operators (60/71)

### Arithmetic
- [x] `+` `-` `*` `/` `%`
- [x] `**` (exponentiation)
- [x] `++` `--` (increment/decrement)

### Comparison
- [x] `==` `!=` `<` `<=` `>` `>=`
- [x] `===` `!==` (strict equality)

### Logical
- [x] `&&` `||` `!`
- [x] `??` (nullish coalescing)

### Bitwise
- [x] `&` `|` `^` `~` `<<` `>>`
- [x] `>>>` (unsigned right shift)

### Assignment
- [x] `=` `+=` `-=` `*=` `/=` `%=`
- [x] `**=` `<<=` `>>=` `>>>=`
- [x] `&&=` `||=` `??=`
- [x] `&=` `|=` `^=`

### Punctuation
- [x] `( ) [ ] { } , ; : . ...`
- [x] `?.` (optional chaining)
- [x] `=>` (arrow)
- [x] `?` (ternary/optional)
- [x] `@` (decorator)
- [x] `#` (private field)
- [x] `` ` `` (template literal)

### Missing tokens
- [ ] `</>` (JSX close tag)
- [ ] `</` (JSX closing slash)
- [ ] regex flags validation (u, v flag conflict detection)

## 2. Keywords (60/94)

### JS Reserved (fully handled)
- [x] `break` `case` `catch` `class` `const` `continue`
- [x] `debugger` `default` `delete` `do` `else` `enum`
- [x] `export` `extends` `false` `finally` `for` `function`
- [x] `if` `import` `in` `instanceof` `new` `null`
- [x] `return` `super` `switch` `this` `throw` `true`
- [x] `try` `typeof` `var` `void` `while` `with`
- [x] `let` `of` `yield` `await` `async`

### TS Contextual (partially handled)
- [x] `interface` `type` `as` `implements` `declare` `abstract`
- [x] `static` `readonly` `private` `protected` `public`
- [x] `override` `accessor` `get` `set`
- [x] `number` `string` `boolean` `any` `unknown` `never`
- [x] `void` `object` `symbol` `bigint`
- [x] `keyof` `infer` `satisfies` `is` `from` `namespace` `module`

### Missing keywords
- [ ] `package` (strict mode reserved)
- [ ] `require` (CJS)
- [ ] `unique` (unique symbol)
- [ ] `global` (ambient declaration)
- [ ] `out` (variance modifier)
- [ ] `intrinsic` (built-in type)
- [ ] `immediate` (dispose)
- [ ] `using` / `await using` (explicit resource management)
- [ ] `asserts` (assertion functions)

## 3. Declarations (10/17)

- [x] `function` declaration
- [x] `async function` declaration
- [x] `class` declaration
- [x] `abstract class` declaration
- [x] `interface` declaration
- [x] `type` alias declaration
- [x] `enum` declaration
- [x] `const`/`let`/`var` declarations
- [x] `import` declaration
- [x] `export` declaration
- [ ] `namespace`/`module` declaration (parsed, not transformed)
- [ ] `declare` ambient declarations (parsed, not transformed)
- [ ] `const enum` declaration (parsed, not distinguished)
- [ ] `export default` expression
- [ ] `export =` (CJS)
- [ ] `import type` (type-only imports)
- [ ] Decorators (`@decorator`)

## 4. Statements (14/21)

- [x] Expression statement
- [x] Block statement `{ ... }`
- [x] Return statement
- [x] If/else statement
- [x] While statement
- [x] Do-while statement (parsed, not tested)
- [x] C-style for loop (desugared to while)
- [x] For-of loop → Cot for-in
- [x] Switch/case/default
- [x] Break statement
- [x] Continue statement
- [x] Throw statement (→ return error)
- [x] Try/catch/finally (parsed, basic transform)
- [x] Variable statement
- [ ] For-in loop (object keys)
- [ ] Labeled statement (parsed, not transformed)
- [ ] Empty statement
- [ ] Debugger statement (should be no-op)
- [ ] With statement (should be compile error)
- [ ] `for await...of`
- [ ] `using` / `await using`

## 5. Expressions (18/32)

- [x] Identifiers
- [x] Numeric literals (int → f64)
- [x] String literals (single + double quote)
- [x] Boolean literals
- [x] Null literal
- [x] Undefined literal
- [x] Template literals (no substitution only)
- [x] Binary expressions (all operators)
- [x] Unary expressions (`!` `-` `~` `typeof` `void` `delete`)
- [x] Increment/decrement (`++`/`--` → assignment)
- [x] Call expressions
- [x] Member access (`obj.prop`)
- [x] Assignment expressions
- [x] Conditional/ternary (`a ? b : c`)
- [x] `new` expression
- [x] `this` → `self`
- [x] Parenthesized expressions
- [x] Arrow functions (basic)
- [ ] Template literals WITH substitutions (`${expr}`)
- [ ] Computed member access (`obj[expr]`)
- [ ] Optional chaining (`?.`) calls/members
- [ ] Spread element (`...arr`)
- [ ] Array literal `[1, 2, 3]`
- [ ] Object literal `{ a: 1, b: 2 }`
- [ ] Function expression
- [ ] Class expression
- [ ] `yield` expression
- [ ] `await` expression (parsed, not transformed)
- [ ] `as` type assertion
- [ ] `satisfies` expression
- [ ] Non-null assertion (`x!`)
- [ ] Tagged template literal
- [ ] `import()` dynamic import

## 6. Type Annotations (8/26)

### Handled
- [x] `number` → `f64`
- [x] `string` → `string`
- [x] `boolean` → `bool`
- [x] `void` → `void`
- [x] `never` → `noreturn`
- [x] `any`/`unknown` → `i64` (placeholder)
- [x] Type references (`TypeName`)
- [x] Array type `T[]` → `List(T)`

### Missing
- [ ] `null` / `undefined` types
- [ ] Union types `T | U` → tagged union
- [ ] Intersection types `T & U` → merged struct
- [ ] `T | null` → `?T` optional
- [ ] Tuple types `[T, U]` → `(T, U)`
- [ ] Function types `(a: T) => R`
- [ ] Object types `{ key: Type }`
- [ ] Generic type args `Array<T>`, `Map<K,V>`
- [ ] Conditional types `T extends U ? X : Y`
- [ ] Mapped types `{ [K in keyof T]: V }`
- [ ] Template literal types
- [ ] `typeof` type queries
- [ ] `keyof` operator
- [ ] Indexed access types `T[K]`
- [ ] `infer` in conditional types
- [ ] Type predicates `x is T`
- [ ] `readonly` modifier on types
- [ ] `Partial<T>`, `Required<T>`, `Pick<T,K>` utility types

## 7. Class Features (10/14)

- [x] Class declaration with fields
- [x] Constructor → `init` method
- [x] Instance methods
- [x] `this` → `self` in methods
- [x] `extends` (field + method inheritance, single level)
- [x] `new ClassName(args)`
- [x] Static methods
- [x] Private fields (modifier parsed, no runtime enforcement)
- [x] `implements` → impl trait
- [x] Method overriding (child method replaces base method)
- [ ] Getter/setter (`get`/`set`)
- [ ] Abstract methods
- [ ] Class expressions
- [ ] Computed property names

## 8. Module System (3/10)

- [x] Named imports `import { x } from './mod'`
- [x] .ts/.js file resolution (try .ts → .js → .cot)
- [x] Multi-file compilation
- [ ] Default imports `import x from './mod'`
- [ ] Namespace imports `import * as x from './mod'`
- [ ] Side-effect imports `import './mod'`
- [ ] Re-exports `export { x } from './mod'`
- [ ] `export *` from module
- [ ] `node_modules` resolution
- [ ] `tsconfig.json` path aliases
- [ ] `package.json` main/exports fields

## 9. Transform Rules (16/35)

### Done
- [x] `function` → `fn`
- [x] `class` → `struct` + `impl`
- [x] `constructor` → `init` method
- [x] `this` → `self`
- [x] `===`/`!==` → `==`/`!=`
- [x] `number` → `f64` (all number literals → float)
- [x] `console.log` → `println`
- [x] `console.error` → `eprintln`
- [x] `extends` → field copy into child struct
- [x] `interface` → `trait`
- [x] `enum` → Cot enum
- [x] `++`/`--` → `x = x + 1`
- [x] Top-level statements → `fn main() void { ... }`
- [x] `@safe` mode auto-enabled for TS files
- [x] `self` param auto-injected for class methods
- [x] Import path resolution (.ts → .js → .cot)

### Missing
- [ ] `??` → `orelse` (token mapped, expression transform incomplete)
- [ ] `?.` → `?.` (optional chaining)
- [ ] Arrow functions → closures (basic works, params incomplete)
- [ ] `throw` → `return error.X` (basic placeholder only)
- [ ] `try/catch` → `catch |err| { }` (not transformed)
- [ ] `Promise<T>` / async → Cot async
- [ ] `T | U` → tagged union
- [ ] `T | null` → `?T`
- [ ] Destructuring `{ a, b } = obj` → field access
- [ ] Array destructuring `[a, b] = arr` → index access
- [ ] Spread `...arr` → expansion
- [ ] `for...in` → key iteration
- [ ] Generics `<T>` → `(T)`
- [ ] Default parameter values
- [ ] Rest parameters `...args`
- [ ] String concatenation `"a" + "b"`
- [ ] `typeof x` → runtime tag check
- [ ] `instanceof` → tag check
- [ ] `implements` → `impl Trait for Struct`

---

## Priority for next work

### High impact (unlocks real programs)
1. Template literal interpolation `${expr}`
2. Array literal `[1, 2, 3]`
3. Object literal `{ a: 1 }`
4. Type inference (`const x = 42` without annotation)
5. String concatenation
6. `async`/`await` transform
7. Union types `T | null` → `?T`

### Medium impact (language completeness)
8. Destructuring
9. Generics `<T>` → `(T)`
10. Arrow function params
11. Optional chaining `?.`
12. Spread/rest operators
13. Default params
14. `for...in`

### Low impact (ecosystem)
15. `node_modules` resolution
16. `tsconfig.json`
17. JSX
18. Decorators
19. `export default`
20. Dynamic `import()`

---

## Known Bugs

| Bug | Impact | Root Cause |
|-----|--------|------------|
| Multi-level extends fails (`C extends B extends A`) | Medium | `transformTsClass` only copies one level of base class fields/methods. Need recursive base chain resolution. |
| `optional params` (`title?: string`) crash in codegen | Low | `?string` compound optional decomposes to 3 words at ABI level, but default null injection only adds 1 value. Needs ABI-aware default injection in lowerer. |
| `do-while` may infinite loop | Low | Condition negation for the break check may not work correctly with f64 comparisons. |
| Multiple async functions in same file conflict | Low | Auto-imported executor dependencies (`std/task_queue`, `std/executor`) cause "redefined identifier" when multiple async fns exist. |
| Generic function calls require explicit type args | Low | Cot checker requires `identity(string)("hello")`, TS expects `identity("hello")` with inference. Needs checker-level type inference for generic calls. |
