structure P = Prelude
structure L = List

fun footer_doc_links l =
  <xml>
  <li style="display:inline">
  {P.head l}
  </li>
  {List.mapX (fn i => <xml>
    <li style="display:inline">·</li>
    <li style="display:inline">
    {i}
    </li>
  </xml>) (P.tail l)}
  </xml>
