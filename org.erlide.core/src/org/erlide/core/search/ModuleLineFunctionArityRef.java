package org.erlide.core.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ModuleLineFunctionArityRef {

	private final String moduleName;
	private final int line;
	private final ErlangFunction function;
	private final String clauseHead;
	private final boolean subClause;
	private final String attribute;

	public ModuleLineFunctionArityRef(final String moduleName, final int line,
			final ErlangFunction function, final String clauseHead,
			final boolean subClause, final String attribute) {
		super();
		this.moduleName = moduleName;
		this.line = line;
		this.subClause = subClause;
		this.function = function;
		this.clauseHead = clauseHead;
		this.attribute = attribute;
	}

	public String getModuleName() {
		return moduleName;
	}

	public int getLine() {
		return line;
	}

	public ErlangFunction getFunction() {
		return function;
	}

	public String getClauseHead() {
		return clauseHead;
	}

	public boolean isSubClause() {
		return subClause;
	}

	public String getAttribute() {
		return attribute;
	}

}
