package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.util.ErlangFunction;

import com.ericsson.otp.erlang.JInterfaceFactory;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlImport extends ErlImportExport implements IErlImport {

	String fImportModule;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlImport(final IErlElement parent, final String importModule,
			final ErlangFunction[] imports) {
		super(parent, "import", imports);
		fImportModule = importModule;
	}

	public ErlImport(final IErlModule parent, final String importModule,
			final OtpErlangList functionList) {
		super(parent, "import", functionList);
		fImportModule = importModule;
	}

	public Kind getKind() {
		return Kind.IMPORT;
	}

	public String getImportModule() {
		return fImportModule;
	}

	@Override
	public String toString() {
		return getName() + ": " + getImportModule();
	}

	@Override
	public OtpErlangObject toErlangObject() {
		final OtpErlangObject funcs = super.toErlangObject();
		return JInterfaceFactory.mkTuple(new OtpErlangAtom(getImportModule()),
				funcs);
	}
}
