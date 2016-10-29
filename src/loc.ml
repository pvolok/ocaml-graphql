type position = {
  line: int;
  column: int;
  offset: int;
}

type filename =
  | ResourceFile of string

type t = {
  source: filename option;
  start: position;
  _end: position;
}

let from_lb_p source start _end = Lexing.(
  {
    source;
    start = {
      line = start.pos_lnum;
      column = start.pos_cnum - start.pos_bol;
      offset = start.pos_cnum;
    };
    _end = {
      line = _end.pos_lnum;
      column = max 0 (_end.pos_cnum - _end.pos_bol);
      offset = _end.pos_cnum;
    }
  }
)
