package org.erlide.core.erlang.internal;

import java.util.ArrayList;
import java.util.List;

import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlImport;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IParent;
import org.erlide.core.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlImport extends ErlMember implements IErlImport, IParent {

	List<ErlangFunction> fFunctions;

	String fImportModule;

	/**
	 * @param parent
	 * @param imports
	 * @param module
	 */
	protected ErlImport(IErlElement parent, String importModule,
			ErlangFunction[] imports) {
		super(parent, "import");
		fImportModule = importModule;
		fFunctions = new ArrayList<ErlangFunction>(imports.length);
		for (ErlangFunction element : imports) {
			fFunctions.add(element);
		}
	}

	public ErlImport(IErlModule parent, String importModule,
			OtpErlangList functionList) {
		super(parent, "import");
		fImportModule = importModule;
		final int n = functionList.arity();
		fFunctions = new ArrayList<ErlangFunction>(n);
		for (int i = 0; i < n; ++i) {
			final OtpErlangTuple function = (OtpErlangTuple) functionList
					.elementAt(i);
			try {
				fFunctions.add(new ErlangFunction(function));
			} catch (final OtpErlangRangeException e) {
				--i;
			}
		}
	}

	public String getElementType() {
		return IMPORT;
	}

	public String getImportModule() {
		return fImportModule;
	}

	public boolean hasImported(ErlangFunction f) {
		return fFunctions.contains(f);
	}

	public ErlangFunction[] getFunctions() {
		return fFunctions.toArray(new ErlangFunction[fFunctions.size()]);
	}

	@Override
	public String toString() {
		return getElementName() + ": " + getImportModule();
	}
}
