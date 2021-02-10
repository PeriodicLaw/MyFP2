# MyFP2

一个玩具级别的函数式语言（带依赖类型）解释器。

## 基本特性

- 依赖类型：

```
let id : (A : Type) -> A -> A
  = \ A \ a a

data Vec : Type -> Nat -> Type where
  | nilv : (A : Type) -> Vec A zero
  | consv : (A : Type) -> (n : Nat) -> A -> Vec A n -> Vec A (suc n)
```

- GADT，以及分类讨论（case splitting）：

```
data Nat : Type where
  | zero : Nat
  | suc : Nat -> Nat

let add : Nat -> Nat -> Nat
  = \ x case x of
        | zero => \ y y
        | suc n => \ y suc (add n y)
```

- 隐式参数（implicit arguments）：

```
data Equal : {A : Type} -> A -> A -> Type where
  | refl : {A} -> {x : A} -> Equal x x

let sym : {A} ->  {x y : A} -> Equal x y -> Equal y x
  = \ e case e of
      | refl => refl

let trans : {A} -> {x y z : A} -> Equal x y -> Equal y z -> Equal x z
  = \ e case e of
      | refl => \ e' case e' of
        | refl => refl
```

- **非常简单的**自定义语法：

```
syntax 0 = zero
syntax 1 = suc (0)
syntax 2 = suc (1)

let fact_1+1=2 : ((1) + (1)) == (2)
  = refl
```

具体例子请见[examples文件夹下的代码](examples/)。

## 咕了

- 修复一下函数展开的bug
- `let-in`表达式
- 带优先级的自定义语法
- 搞个标准库