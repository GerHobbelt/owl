timer () {
	START=$(bin/vm fasl/init.fasl -e '(time-ms)')
	"$@"
	shift $(($# - 1))
	bin/vm fasl/init.fasl -e '(str"bootstrap: "(-(time-ms)'$START')" ms\nfasl: "(file-size"'"$1"'")" b")'
}

case $1 in
(-f)
	shift
	while :
	do
		# check that the compiling image passes tests
		sh tests/run all bin/vm fasl/boot.fasl
		# compile fasl
		timer "$@"
		# copy new image to ol.fasl, if it is a fixed point
		cmp -s fasl/boot.fasl fasl/bootp.fasl && exec mv fasl/bootp.fasl fasl/ol.fasl
		# otherwise recompile
		mv fasl/bootp.fasl fasl/boot.fasl
	done
	;;
(*)
	timer "$@"
esac
