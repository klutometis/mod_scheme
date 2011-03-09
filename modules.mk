mod_scheme.la: mod_scheme.slo
	$(SH_LINK) -rpath $(libexecdir) -module -avoid-version  mod_scheme.lo
DISTCLEAN_TARGETS = modules.mk
shared =  mod_scheme.la
