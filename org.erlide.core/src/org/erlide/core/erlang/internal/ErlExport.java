package org.erlide.core.erlang.internal;

import org.erlide.core.erlang.IErlExport;
import org.erlide.core.erlang.IErlModule;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

	protected ErlExport(final IErlModule module,
			final OtpErlangList functionList) {
		super(module, "export", functionList);
	}

	public Kind getKind() {
		return Kind.EXPORT;
	}

	@Override
	public String toString() {
		return getName();
	}

}
