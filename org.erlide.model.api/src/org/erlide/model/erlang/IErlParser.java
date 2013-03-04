package org.erlide.model.erlang;

public interface IErlParser {

	public abstract boolean parse(final IErlModule module,
			final String scannerName, final boolean initialParse,
			final String path, String initialText, boolean updateSearchServer);

}
