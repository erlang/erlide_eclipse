package com.ericsson.otp.erlang;

import java.io.Serializable;

public class OtpErlangFun extends OtpErlangObject implements Serializable {
    // don't change this!
    private static final long serialVersionUID = -3423031125356706472L;

    private final OtpErlangPid pid;
    private final String module;
    private final long index;
    private final long old_index;
    private final long uniq;
    private final OtpErlangObject[] freeVars;
    private final int arity;
    private final byte[] md5;

    public OtpErlangFun(final OtpInputStream buf)
	    throws OtpErlangDecodeException {
	final OtpErlangFun f = buf.read_fun();
	pid = f.pid;
	module = f.module;
	arity = f.arity;
	md5 = f.md5;
	index = f.index;
	old_index = f.old_index;
	uniq = f.uniq;
	freeVars = f.freeVars;
    }

    public OtpErlangFun(final OtpErlangPid pid, final String module,
	    final long index, final long uniq, final OtpErlangObject[] freeVars) {
	this.pid = pid;
	this.module = module;
	arity = -1;
	md5 = null;
	this.index = index;
	old_index = 0;
	this.uniq = uniq;
	this.freeVars = freeVars;
    }

    public OtpErlangFun(final OtpErlangPid pid, final String module,
	    final int arity, final byte[] md5, final int index,
	    final long old_index, final long uniq,
	    final OtpErlangObject[] freeVars) {
	this.pid = pid;
	this.module = module;
	this.arity = arity;
	this.md5 = md5;
	this.index = index;
	this.old_index = old_index;
	this.uniq = uniq;
	this.freeVars = freeVars;
    }

    @Override
    public void encode(final OtpOutputStream buf) {
	buf
		.write_fun(pid, module, old_index, arity, md5, index, uniq,
			freeVars);
    }

    @Override
    public boolean equals(final Object o) {
	if (!(o instanceof OtpErlangFun)) {
	    return false;
	}
	final OtpErlangFun f = (OtpErlangFun) o;
	if (!pid.equals(f.pid) || !module.equals(f.module) || arity != f.arity) {
	    return false;
	}
	if (md5 == null) {
	    if (f.md5 != null) {
		return false;
	    }
	} else {
	    if (!md5.equals(f.md5)) {
		return false;
	    }
	}
	if (index != f.index || uniq != f.uniq) {
	    return false;
	}
	if (freeVars == null) {
	    return f.freeVars == null;
	}
	return freeVars.equals(f.freeVars);
    }

    @Override
    public String toString() {
	return "#Fun<" + module + "." + old_index + "." + uniq + ">";
    }

}
