package org.erlide.core.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ModuleLineFunctionArityRef {

	private final String moduleName;
	private final int line;
	private final ErlangFunction function;
	private final String clauseHead;
	private final boolean subClause;

	public ModuleLineFunctionArityRef(final String moduleName, final int line,
			final String function, final int arity, final String clauseHead,
			final boolean subClause) {
		super();
		this.moduleName = moduleName;
		this.line = line;
		this.subClause = subClause;
		this.function = new ErlangFunction(function, arity);
		this.clauseHead = clauseHead;
	}

	public String getModuleName() {
		return moduleName;
	}

	public int getLine() {
		return line;
	}

	public String getFunctionName() {
		return function.name;
	}

	public int getArity() {
		return function.arity;
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

}
