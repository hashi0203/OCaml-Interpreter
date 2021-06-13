# OCaml Interpreter by OCaml

## OCaml のインストール

- Ubuntu の場合

    ```bash
    $ sudo apt install ocaml
    $ sudo apt install install rlwrap
    # OCaml インタプリタで上下キーで履歴が打てるようにする (デフォルトではサポートされていない)
    ```

- Mac の場合

    ```bash
    $ brew install ocaml
    $ brew install install rlwrap
    # OCaml インタプリタで上下キーで履歴が打てるようにする (デフォルトではサポートされていない)
    ```

`rlwrap` はなくてもいいですが，OCaml インタプリタでデフォルトでは上下キーで履歴が打てないので，これを入れることで履歴を辿れるようにする．

その他の場合は [公式ページ](https://ocaml.org/docs/install.html) を見てください．


## 動かし方

- 準備

    ```bash
    $ git clone https://github.com/hashi0203/OCaml-Compiler.git
    $ cd OCaml-Compiler
    $ make
    ```

- 起動

    ```bash
    $ rlwrap ./main
    ```

- 終了

    実装していないので，`Ctrl` + `c` でお願いします．

- 不要な実行ファイルを削除

    ```bash
    $ make clean
    ```

## 実行可能なプログラム

これが全てというわけではないですが，一例として載せておきます．

- 整数の四則演算
    ```ocaml
    # (8 - 2) / 3;;
    - : int = 2
    # 3 * 3 / 2;;
    - : int = 4
    # 8 / 0;;
    EvalErr
    ```

    - 割り算は切り捨てた結果が出ます．


- 変数の定義 (`let`)

    ```ocaml
    # x;;
    - : int = 10
    # i;;
    - : int = 1
    # v;;
    - : int = 5
    # let x = 2;;
    x : int = 2
    # x;;
    x : int = 2 # 更新されている
    # let x = if x < 5 then i else v in x;;
    - : int = 1
    # x;;
    - : int = 2 # 更新されていない
    # let x = if x < 5 then i else v;;
    x : int = 1
    # x;;
    - : int = 1 # 更新されている
    ```

    - デフォルトで x, i, v にはそれぞれ 10, 1, 5 が代入されている．
    - `let x = e;;` で更新すると値は `e` に更新される
    - `let x = e1 in e;;` では環境は更新されないため，`x` の値自体は更新されない．

- 関数の定義・適用 (`fun`)

    ```ocaml
    # fun x -> x;;
    - : a1 -> a1 = <fun>
    # (fun x -> x) 4;;
    - : int = 4
    # fun x -> if x < 5 then x else 5;;
    - : int -> int = <fun>
    # (fun x -> if x < 5 then x else 5) 4;;
    - : int = 4
    # (fun x -> if x < 5 then x else 5) 7;;
    - : int = 5
    # let a = 5;;
    a : int = 5
    # let f = fun x -> if x < a then x else a;;
    f : int -> int = <fun>
    # let a = 10;;
    a : int = 10
    # f 4;;
    - : int = 4 # a = 5 の状態で評価されている
    # f 7;;
    - : int = 5 # a = 5 の状態で評価されている
    # (fun x -> x) true;;
    - : bool = true
    # let f = fun x -> x;;
    f : a14 -> a14 = <fun>
    # f 3;;
    - : int = 3 # int を引数にしても評価できる
    # f true;;
    - : bool = true # bool を引数にしても評価できる
    # let x = if v < 4 then f 3 else f true;;
    TypeErr # if の中身が異なる型になると型推論でエラーが出る
    ```

    - 型に合わせて関数の定義や適用ができる．
    - 最後の例からも型検査がしっかりできていることがわかる．


- 再帰関数の定義・適用 (`let rec`)

    ```ocaml
    # let rec fact x = if x = 1 then 1 else x * fact (x-1);;
    fact : int -> int = <fun>
    # fact 5;;
    - : int = 120
    # fact true;;
    TypeErr # 型検査がしっかりできている．
    # let rec fact x = if x = 1 then 1 else x * fact (x-1) in fact 5;;
    - : int = 120
    ```

    - クロージャが環境の参照を取るようにして関数の再帰的定義を実装した．

- ペア関数の実装

    ```ocaml
    # let pair = fun a -> fun b -> fun s -> (s a b);;
    pair : a4 -> a5 -> a4 -> a5 -> a8 -> a8 = <fun>
    # let tru = fun x -> fun y -> x;;
    tru : a9 -> a10 -> a9 = <fun>
    # let fls = fun x -> fun y -> y;;
    fls : a11 -> a12 -> a12 = <fun>
    # pair 1 2 tru;;
    - : int = 1 # tru にすると一つ目の要素が取り出せる
    # pair 1 2 fls;;
    - : int = 2 # fls にすると二つ目の要素が取り出せる
    ```

- 組型とリスト型

    ```ocaml
    # (1 + 2 * 3, (2 - 1, 4 / 2));;
    - : int * int * int = (7, (1, 2))
    # [];;
    - : a1 list = []
    # (1, true) :: [];;
    - : int * bool list = [(1, true)]
    # (1,1) :: (2,2) :: [];;
    - : int * int list = [(1, 1); (2, 2)]
    # (1,1) :: (2,true) :: [];;
    TypeErr
    # let f = fun x -> x;;
    f : a5 -> a5 = <fun>
    # (f 0, f true);;
    TypeErr # 多相には対応していない
    ```

    - ネストした組なども問題なく使える．
    - 組は左右の型が違っていても評価可能だが，リストは全ての要素が同じ型でないといけない．
    - let 多相には対応していないため，最後の式は実行できない．

- パターンマッチ

    ```ocaml
    # match (1,2) with (a,b) -> a + b;;
    - : int = 3
    # let f = fun a -> match a with x -> x;;
    f : a6 -> a6 = <fun>
    # f true;;
    - : bool = true
    # f 1;;
    - : int = 1
    # match [] with a -> a;;
    - : a10 list = []
    # let rec f x = match x with [] -> 0 | x :: xs -> x + f xs;;
    f : int list -> int = <fun>
    # f (1 :: 2 :: 3 :: []);;
    - : int = 6
    # match 1 with 0 -> 0;;
    EvalErr
    # match 1 with 1 -> true | x -> x;;
    TypeErr
    # match 1 with 1 -> 1 | true -> true;;
    TypeErr
    # match (1,true) with (true,1) -> true;;
    TypeErr
    # match 1 :: [] with true :: xs -> true;;
    TypeErr
    ```

    - `match e with` としたとき，`e` とあうパターンを探して，見つかればそのパターンを評価し，当てはまるパターンが1つもなければ `EvalErr` を返す．
    - 返り値の型が全て同じでない場合や `match e with` の `e` とパターンの型が異なっている場合などは `TypeErr` を返す．

- 型推論

    - 各処理が実行される前に型検査を行っている．
        - 四則演算の `8 / 0;;` は型はあっているがゼロ除算のため実行できず `EvalErr`
        - 再帰関数の `fact true;;` は型がそもそも違っているので型推論を通らず `TypeErr`
    - 型推論の結果は最も一般的な型 (最汎単一化子) を返している．