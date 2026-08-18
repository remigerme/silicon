#!/bin/bash

SILICON_UPSTREAM="../../silicon-upstream/target/scala-2.13/silicon.jar"
SILICON_FORK="../target/scala-2.13/silicon.jar"

JAVA_ARGS="-Xss32m"
SILICON_ARGS="--parallelizeBranches"
ROUTER="--assumeInjectivityOnInhale --exhaleMode=1 --conditionalizePermissions --select=resolveLocalDst_163f8674_PMDataPlane,addEndhostPort_163f8674_F router.vpr"
MESSAGE="--select=findEntriesWithInv_c13ece1f_PMTraceManager,findEntryWithInv_c13ece1f_PMTraceManager,findEvents_c13ece1f_PMTraceManager message-lemmas.gobra.vpr"

CMD_UPSTREAM="java $JAVA_ARGS -jar $SILICON_UPSTREAM $SILICON_ARGS"
CMD_FORK="java $JAVA_ARGS -jar $SILICON_FORK $SILICON_ARGS"

hyperfine --runs 3 "$CMD_UPSTREAM $ROUTER" "$CMD_FORK $ROUTER" --export-markdown bench-results-router.md
hyperfine --runs 10 "$CMD_UPSTREAM $MESSAGE" "$CMD_FORK $MESSAGE" --export-markdown bench-results-message.md
