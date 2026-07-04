# Specify a vew variables to avoid repitition downstream
# https://stackoverflow.com/questions/46741739/how-to-use-makefiles-with-r-cmd-build

DATADIR := docs/data
SCRIPTDIR := inst/scripts
EXTS := qs rds

DATASETS := \
    fpsim-alliances-a-unweighted \
    fpsim-alliances-a-weighted \
    fpsim-alliances-pk \
    fpsim-alliances-s-unweighted \
    fpsim-alliances-s-weighted \
    fpsim-alliances-taub \
    fpsim-votes-a \
    fpsim-votes-pk

TARGETS := $(foreach d,$(DATASETS),\
    $(addprefix $(DATADIR)/$(d).,$(EXTS)))

all: $(TARGETS)

$(DATADIR)/%.qs $(DATADIR)/%.rds: $(SCRIPTDIR)/%.R
	Rscript $<
