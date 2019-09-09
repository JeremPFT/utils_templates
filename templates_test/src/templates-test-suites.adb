with Templates.Tests;
with Ada.Text_IO;

package body Templates.Test.Suites is

  pragma Check_Policy (Debug, On); -- -gnata

  package T_IO renames Ada.Text_IO;

  pragma Debug ( T_IO.Put_Line ("templates.test.suites") );
  Result : aliased Aunit.Test_Suites.Test_Suite;

  Test_Template : aliased Templates.Tests.Test_Case;

  function Suite
    return Aunit.Test_Suites.Access_Test_Suite
  is
    use Aunit.Test_Suites;
    Template : constant access Templates.Tests.Test_Case := Test_Template'Access;
  begin
    Add_Test (Result'Access, Template);

    return Result'Access;
  end Suite;

end Templates.Test.Suites;
