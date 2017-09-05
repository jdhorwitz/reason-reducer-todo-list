type item = {
  id: int,
  title: string,
  completed: bool
};

let se = ReasonReact.stringToElement;

module TodoItem = {
  let component = ReasonReact.statelessComponent "TodoItem";
  let make ::item ::onToggle _children => {
    ...component,
    render: fun _ =>
      <div className="item" onClick=(fun _evt => onToggle ())>
        <input _type="checkbox" checked=(Js.Boolean.to_js_boolean item.completed) />
        (se item.title)
      </div>
  };
};

let valueFromEvent evt :string => (evt |> ReactEventRe.Form.target |> ReactDOMRe.domElementToObj)##value;

module Input = {
  type state = string;
  type actions =
    | AddText string;
  let component = ReasonReact.reducerComponent "Input";
  let make _ => {
    ...component,
    initialState: fun () => "",
    reducer: fun action _ =>
      switch action {
      | AddText text => ReasonReact.Update text
      },
    render: fun {state: text, reduce} =>
      <input
        value=text
        _type="text"
        placeholder="Write something to do"
        onChange=(reduce (fun evt => AddText (valueFromEvent evt)))
      />
  };
};

type state = {items: list item};

let component = ReasonReact.reducerComponent "TodoApp";

let lastId = ref 0;

let newItem text => {
  lastId := !lastId + 1;
  {id: !lastId, title: text, completed: false}
};

type actions =
  | NewItem string
  | ToggleItem item;

let toggleItem items id =>
  List.map (fun item => item.id === id ? {...item, completed: not item.completed} : item) items;

let make _children => {
  ...component,
  initialState: fun () => {items: [{id: 0, title: "Write some things to do", completed: false}]},
  reducer: fun action state =>
    switch action {
    | NewItem text => ReasonReact.Update {...state, items: [newItem text, ...state.items]}
    | ToggleItem item => ReasonReact.Update {...state, items: toggleItem state.items item.id}
    },
  render: fun {state, reduce} => {
    let numItems = List.length state.items;
    let plural = numItems == 1 ? "Item" : "Items";
    <div className="app">
      <div className="title"> (se "What to do") <Input /> </div>
      <div className="items">
        (
          ReasonReact.arrayToElement (
            Array.of_list (
              List.map
                (
                  fun item =>
                    <TodoItem
                      key=(string_of_int item.id)
                      onToggle=(reduce (fun _ => ToggleItem item))
                      item
                    />
                )
                state.items
            )
          )
        )
      </div>
      <div className="footer"> (se (string_of_int numItems)) (se plural) </div>
    </div>
  }
};