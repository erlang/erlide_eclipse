package com.ericsson.otp.erlang;

public class OtpErlangExternalFun extends OtpErlangObject {
    // don't change this!
    private static final long serialVersionUID = 6443965570641913886L;

    private final String module;
    private final String function;
    private final int arity;

    public OtpErlangExternalFun(final String module, final String function,
	    final int arity) {
	super();
	this.module = module;
	this.function = function;
	this.arity = arity;
    }

    public OtpErlangExternalFun(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	final OtpErlangExternalFun f = buf.read_external_fun();
	module = f.module;
	function = f.function;
	arity = f.arity;
    }

    @Override
    public void encode(final OtpOutputStream buf) {
	buf.write_external_fun(module, function, arity);
    }

    @Override
    public boolean equals(final Object o) {
	if (!(o instanceof OtpErlangExternalFun)) {
	    return false;
	}
	final OtpErlangExternalFun f = (OtpErlangExternalFun) o;
	return module.equals(f.module) && function.equals(f.function)
		&& arity == f.arity;
    }

    @Override
    public String toString() {
	return "#Fun<" + module + "." + function + "." + arity + ">";
    }

}
