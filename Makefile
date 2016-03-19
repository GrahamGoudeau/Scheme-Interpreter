BASIS_FILE=interpreter.mlb
COMPILER=mlton
COMP_FLAGS=-codegen native
all:
		${COMPILER} ${COMP_FLAGS} ${BASIS_FILE}
