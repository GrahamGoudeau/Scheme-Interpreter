BASIS_FILE=scheme.mlb
COMPILER=mlton
COMP_FLAGS=-codegen native
all:
		${COMPILER} ${COMP_FLAGS} ${BASIS_FILE}
