package org.erlide.ui.search;

import org.eclipse.ui.dialogs.SearchPattern;

public class ErlSearchPattern extends SearchPattern {
	private final int searchFor;
	private final int limitTo;
	private final int ruleExactMatch;
	private final String pattern;

	/**
	 * @param pattern
	 * @param searchFor
	 * @param limitTo
	 * @param ruleExactMatch
	 * @return
	 */
	public static SearchPattern createPattern(final String pattern,
			final int searchFor, final int limitTo, final int ruleExactMatch) {
		return new ErlSearchPattern(pattern, searchFor, limitTo, ruleExactMatch);
	}

	/**
	 * @param pattern
	 * @param searchFor
	 * @param limitTo
	 * @param ruleExactMatch
	 */
	public ErlSearchPattern(final String pattern, final int searchFor,
			final int limitTo, final int ruleExactMatch) {
		super();
		this.pattern = pattern;
		this.searchFor = searchFor;
		this.limitTo = limitTo;
		this.ruleExactMatch = ruleExactMatch;
	}

	/**
	 * @return the searchFor
	 */
	public int getSearchFor() {
		return searchFor;
	}

	/**
	 * @return the limitTo
	 */
	public int getLimitTo() {
		return limitTo;
	}

	/**
	 * @return the ruleExactMatch
	 */
	public int getRuleExactMatch() {
		return ruleExactMatch;
	}

	/**
	 * @return the pattern
	 */
	@Override
	public String getPattern() {
		return pattern;
	}
}
