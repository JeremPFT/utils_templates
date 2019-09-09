with Templates;

package Templates.Test.Expected is

  Directory : constant String :=
    "d:/Users/jpiffret/AppData/Roaming/Dropbox/projets_perso/ada/templates/templates_test/tests";

  Template_1_Filename : constant String := "template_1.ttxt";
  Template_1_Tag_1    : constant String := "tmpl_1_tag_1";
  Template_1_Tag_2    : constant String := "tmpl_1_tag_2";

  Template_1_Tag_1_Value_1 : constant Integer := 1;
  Template_1_Tag_1_Value_2 : constant Integer := 2;

  Template_1_Tag_1_Value_1_Img : constant String := "1";
  Template_1_Tag_1_Value_2_Img : constant String := "2";

  Template_1_Tag_2_Value_1 : constant String := "one";
  Template_1_Tag_2_Value_2 : constant String := "two";

  function Template_1 return not null access Templates.Object_T;

end Templates.Test.Expected;
