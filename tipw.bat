@ECHO OFF

ECHO "Compiling..."
CALL gradlew.bat installDist > comp.out
if %ERRORLEVEL% NEQ 0 (
  ECHO "Compilation failed"
  ECHO %comp.out%
  EXIT 1
)
CALL build\install\TIP\bin\TIP.bat %*
