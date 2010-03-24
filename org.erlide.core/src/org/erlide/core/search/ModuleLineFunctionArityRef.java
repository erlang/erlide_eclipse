package org.erlide.core.search;

import org.erlide.core.erlang.util.ErlangFunction;

public class ModuleLineFunctionArityRef {

	private final String moduleName;
	private final int offset, length;
	private final ErlangFunction function;
	private final String clauseHead;
	private final boolean subClause;
	private final String attribute;

	public ModuleLineFunctionArityRef(final String moduleName,
			final int offset, final int length, final ErlangFunction function,
			final String clauseHead, final boolean subClause,
			final String attribute) {
		this.moduleName = moduleName;
		this.offset = offset;
		this.length = length;
		this.subClause = subClause;
		this.function = function;
		this.clauseHead = clauseHead;
		this.attribute = attribute;
	}

	public String getModuleName() {
		return moduleName;
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

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return offset;
	}

}
