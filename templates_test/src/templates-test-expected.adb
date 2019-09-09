package body Templates.Test.Expected is

  function Template_1
    return not null access Templates.Object_T
  is
    Result : access Templates.Object_T;
  begin
    Result := Templates.Create
      (Template_Directory_Name => Directory,
       Template_File_Name      => Template_1_Filename,
       Tag_Names               => (+Template_1_Tag_1,
                                   +Template_1_Tag_2));
    return Result;
  end Template_1;

end Templates.Test.Expected;
