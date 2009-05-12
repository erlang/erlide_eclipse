package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.erlang.util.ErlangFunction;

import com.ericsson.otp.erlang.JInterfaceFactory;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class ErlImportExport extends ErlMember implements IParent {

	protected List<ErlangFunction> fFunctions;

	protected ErlImportExport(final IErlElement parent, final String name,
			final ErlangFunction[] imports) {
		super(parent, name);
		fFunctions = new ArrayList<ErlangFunction>(imports.length);
		for (final ErlangFunction element : imports) {
			fFunctions.add(element);
		}
	}

	public ErlImportExport(final IErlModule module, final String name,
			final OtpErlangList functionList) {
		super(module, name);
		fFunctions = new ArrayList<ErlangFunction>(functionList.arity());
		for (final OtpErlangObject i : functionList.elements()) {
			final OtpErlangTuple function = (OtpErlangTuple) i;
			fFunctions.add(new ErlangFunction(function));
		}
	}

	public boolean hasFunction(final ErlangFunction f) {
		return fFunctions.contains(f);
	}

	public List<ErlangFunction> getFunctions() {
		return fFunctions;
	}

	public OtpErlangObject toErlangObject() {
		final OtpErlangObject[] funcTuples = new OtpErlangObject[fFunctions
				.size()];
		int i = 0;
		for (final ErlangFunction f : fFunctions) {
			funcTuples[i++] = JInterfaceFactory.mkTuple(new OtpErlangAtom(
					f.name), new OtpErlangLong(f.arity));
		}
		return new OtpErlangList(funcTuples);
	}

}
