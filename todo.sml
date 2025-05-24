(* Todoリストアプリケーション *)

(* データ型の定義 *)
datatype status = Pending | Completed;

datatype todo = Todo of {
    title: string,
    description: string,
    dueDate: string,
    status: status
};

(* Todoリストの型定義 *)
type todoList = todo list;

(* 空のTodoリスト *)
val emptyList: todoList = [];

(* Todoアイテムの作成 *)
fun createTodo (title: string, description: string, dueDate: string) : todo =
    Todo {
        title = title,
        description = description,
        dueDate = dueDate,
        status = Pending
    };

(* Todoリストにアイテムを追加 *)
fun addTodo (list: todoList, newTodo: todo) : todoList =
    newTodo :: list;

(* Todoアイテムの削除 *)
fun removeTodo (list: todoList, index: int) : todoList =
    let
        fun remove (l: todoList, i: int, acc: todoList) : todoList =
            case (l, i) of
                ([], _) => rev acc
              | (x::xs, 0) => rev acc @ xs
              | (x::xs, n) => remove (xs, n-1, x::acc)
    in
        remove (list, index, [])
    end;

(* Todoアイテムの更新 *)
fun updateTodo (list: todoList, index: int, newTodo: todo) : todoList =
    let
        fun update (l: todoList, i: int, acc: todoList) : todoList =
            case (l, i) of
                ([], _) => rev acc
              | (x::xs, 0) => rev acc @ (newTodo::xs)
              | (x::xs, n) => update (xs, n-1, x::acc)
    in
        update (list, index, [])
    end;

(* Todoアイテムの状態を更新 *)
fun updateStatus (list: todoList, index: int, newStatus: status) : todoList =
    let
        fun update (l: todoList, i: int, acc: todoList) : todoList =
            case (l, i) of
                ([], _) => rev acc
              | (Todo {title, description, dueDate, status}::xs, 0) =>
                    rev acc @ (Todo {title=title, description=description, dueDate=dueDate, status=newStatus}::xs)
              | (x::xs, n) => update (xs, n-1, x::acc)
    in
        update (list, index, [])
    end;

(* Todoリストの表示 *)
fun displayTodo (Todo {title, description, dueDate, status}) : unit =
    let
        val statusStr = case status of
            Pending => "未完了"
          | Completed => "完了"
    in
        print ("タイトル: " ^ title ^ "\n");
        print ("説明: " ^ description ^ "\n");
        print ("期限: " ^ dueDate ^ "\n");
        print ("状態: " ^ statusStr ^ "\n");
        print "-------------------\n"
    end;

fun displayList (list: todoList) : unit =
    let
        fun display (l: todoList, index: int) : unit =
            case l of
                [] => ()
              | x::xs => (
                    print ("[" ^ Int.toString index ^ "] ");
                    displayTodo x;
                    display (xs, index + 1)
                )
    in
        display (list, 0)
    end;

(* 完了済みのTodoをフィルタリング *)
fun filterCompleted (list: todoList) : todoList =
    List.filter (fn (Todo {status, ...}) => status = Completed) list;

(* 未完了のTodoをフィルタリング *)
fun filterPending (list: todoList) : todoList =
    List.filter (fn (Todo {status, ...}) => status = Pending) list;

(* テスト用のサンプルデータ *)
val sampleList = [
    createTodo ("買い物", "牛乳とパンを買う", "2024-03-20"),
    createTodo ("レポート", "SMLの課題を提出", "2024-03-25"),
    createTodo ("ミーティング", "プロジェクトの進捗報告", "2024-03-22")
];

(* メイン関数 *)
fun main () : unit =
    let
        val list1 = addTodo (emptyList, createTodo ("テスト", "テスト用のTodo", "2024-03-21"))
        val list2 = addTodo (list1, createTodo ("テスト2", "2つ目のテスト", "2024-03-22"))
        val list3 = updateStatus (list2, 0, Completed)
    in
        print "=== すべてのTodo ===\n";
        displayList list3;
        print "\n=== 完了済みのTodo ===\n";
        displayList (filterCompleted list3);
        print "\n=== 未完了のTodo ===\n";
        displayList (filterPending list3)
    end;
