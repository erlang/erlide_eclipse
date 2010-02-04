
-record(erlide_token, {module, kind=eof, line=1, offset=1, length=0, value=eof, text=""}).

-record(erlide_form, {module, kind, start=0, stop=0, tree}).
