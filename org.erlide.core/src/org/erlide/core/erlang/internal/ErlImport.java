package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlImport extends ErlMember implements IErlImport, IParent {

	List<ErlangFunction> fFunctions;

	String fImportModule;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlImport(final IErlElement parent, final String importModule,
			final ErlangFunction[] imports) {
		super(parent, "import");
		fImportModule = importModule;
		fFunctions = new ArrayList<ErlangFunction>(imports.length);
		for (final ErlangFunction element : imports) {
			fFunctions.add(element);
		}
	}

	public ErlImport(final IErlModule parent, final String importModule,
			final OtpErlangList functionList) {
		super(parent, "import");
		fImportModule = importModule;
		fFunctions = new ArrayList<ErlangFunction>(functionList.arity());
		for (final OtpErlangObject i : functionList.elements()) {
			final OtpErlangTuple function = (OtpErlangTuple) i;
			fFunctions.add(new ErlangFunction(function));
		}
	}

	public Kind getKind() {
		return Kind.IMPORT;
	}

	public String getImportModule() {
		return fImportModule;
	}

	public boolean hasImported(final ErlangFunction f) {
		return fFunctions.contains(f);
	}

	public List<ErlangFunction> getFunctions() {
		return fFunctions;
	}

	@Override
	public String toString() {
		return getName() + ": " + getImportModule();
	}

	public OtpErlangObject toErlangObject() {
		final List<ErlangFunction> impFuncs = getFunctions();
		final OtpErlangObject[] rImpFuncs = new OtpErlangObject[impFuncs.size()];
		int i = 0;
		for (final ErlangFunction f : impFuncs) {
			rImpFuncs[i++] = new OtpErlangTuple(new OtpErlangAtom(f.name),
					new OtpErlangLong(f.arity));
		}
		return new OtpErlangTuple(new OtpErlangAtom(getImportModule()),
				new OtpErlangList(rImpFuncs));
	}
}
