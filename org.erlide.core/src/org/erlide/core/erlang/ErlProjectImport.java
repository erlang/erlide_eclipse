package org.erlide.core.erlang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.erlide.jinterface.backend.util.Util;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlProjectImport {
	private final Set<String> resources;
	private final List<String> sourceDirs;
	private final List<String> includeDirs;

	public ErlProjectImport(final OtpErlangObject o) {
		final OtpErlangTuple t = (OtpErlangTuple) o;
		OtpErlangList l = (OtpErlangList) t.elementAt(0);
		resources = (Set<String>) erlangStringList2Collection(l,
				new HashSet<String>());
		l = (OtpErlangList) t.elementAt(1);
		sourceDirs = (List<String>) erlangStringList2Collection(l,
				new ArrayList<String>());
		l = (OtpErlangList) t.elementAt(2);
		includeDirs = (List<String>) erlangStringList2Collection(l,
				new ArrayList<String>());
	}

	private static Collection<String> erlangStringList2Collection(
			final OtpErlangList l, final Collection<String> c) {
		for (final OtpErlangObject o : l.elements()) {
			c.add(Util.stringValue(o));
		}
		return c;
	}

	public Collection<String> getResources() {
		return resources;
	}

	public Collection<String> getSourceDirs() {
		return sourceDirs;
	}

	public Collection<String> getIncludeDirs() {
		return includeDirs;
	}
}
