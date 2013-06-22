
-define(_FSM_TEST(Id, Title, CmdList),
  {Title, fun() -> [ eunit_fsm:translate_cmd(Id, Cmd) || Cmd <- CmdList] end}).