Since Delphi does not natively supports AVX instructions we need
to convert these instructions to DB statements.

The process to create new AVX instructions based code
is to first develop the assembler based code in FPC.

-> Use Codetyphoon for this purpose and the MathUtilsTest32/64 (or MathUtilsTestLinux)
projects for this purpose.

Compile all the projects without debug information and optimization o3 on.

-> Make sure the compiled object files are in the .\Test\lib\i386-win32
and the .\Test\lib\x86_64-win64 folders or adjust the paths in the 
processPasFiles.bat file.

-> download the tool object file converter tool from http://www.agner.org/optimize/
and place the exe file in this directory.


-> compile the AVXPortToDelphi.dpr project.
-> run processPasFiles.bat

-> DONE


The batch file invokes the objeconv.exe tool and strips out the assembler op codes
These opcodes are then placed in the pas files via IFDEFS. Note that if you change some
assembler instructions in FPC all you need is to run the tool again and the IFDEFS get updated.
   
##############################################
Some problems that one needs to keep in mind.
-> Delphi and FPC use different stack alignments and local variables mostly get hinted out.
-> Same is for global variables. Always use the lea (load effective address) instruction to first load the true destination 
   into a register before you use the value with AVX instructions.
-> Local variables layout is somehow different (caused nasty AV's here in the first place). 
   Try to either use lea first or manipulate the stack manually aka refer to fixed stack positions 
   like in AVXMatrixVectorMultOperations.AVXMatrixVectMultT .




