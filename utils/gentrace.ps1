# Powershell script for generating PIN traces

$SCRIPTPATH = split-path -parent $MyInvocation.MyCommand.Definition
$BAPDIR = join-path $SCRIPTPATH ".."
$PINDIR = join-path $BAPDIR "pin"
$PIN = join-path $PINDIR "pin"

$TOOL32 = join-path $BAPDIR "pintraces/obj-ia32/gentrace.dll"
$TOOL64 = join-path $BAPDIR "pintraces/obj-intel64/gentrace.dll"

if ($args.count -eq 0) {
	"Usage: " + $MyInvocation.MyCommand.Definition + " <arguments to PIN tool>"
	Return 1
}

if (!(test-path $TOOL32)) {
    "The BAP pintool does not seem to be built."
    "See the BAP user manual."
    Return 1
}

if (!(test-path $PINDIR)) {
	"PIN does not appear to be installed in " + $PINDIR
	Return 1
}

$ARGS = @("-t", $TOOL32) + $args
& $PIN $ARGS
$SUCCESS = $LastExitCode
if ($SUCCESS -ne 0 -and (test-path $TOOL64)) {
	$ARGS = @("-t", $TOOL64) + $args
	& $PIN $ARGS
	$SUCCESS = $LastExitCode
}

Return $SUCCESS

