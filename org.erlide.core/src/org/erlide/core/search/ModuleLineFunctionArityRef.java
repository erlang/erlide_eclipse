package org.erlide.core.search;

public class ModuleLineFunctionArityRef {

	private final String moduleName;
	private final int offset, length;
	private final String name;
	private final int arity;
	private final String clauseHead;
	private final boolean subClause;

	public ModuleLineFunctionArityRef(final String moduleName,
			final int offset, final int length, final String name,
			final int arity, final String clauseHead, final boolean subClause) {
		this.moduleName = moduleName;
		this.offset = offset;
		this.length = length;
		this.subClause = subClause;
		this.name = name;
		this.arity = arity;
		this.clauseHead = clauseHead;
	}

	public String getModuleName() {
		return moduleName;
	}

	public String getName() {
		return name;
	}

	public String getClauseHead() {
		return clauseHead;
	}

	public boolean isSubClause() {
		return subClause;
	}

	public int getLength() {
		return length;
	}

	public int getOffset() {
		return offset;
	}

	public int getArity() {
		return arity;
	}

	public boolean isMatch() {
		return false;
	}
}
