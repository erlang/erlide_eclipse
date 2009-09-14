/**
 * 
 */
package org.erlide.ui.internal;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendEvalResult;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

class MonitorEntry {

	public static class DeltaMap {
		public String label = "?";
		public List<Map<String, OtpErlangObject>> added = new ArrayList<Map<String, OtpErlangObject>>();
		public List<Map<String, OtpErlangObject>> deleted = new ArrayList<Map<String, OtpErlangObject>>();
		public List<Map<String, OtpErlangObject>> modified = new ArrayList<Map<String, OtpErlangObject>>();
	}

	public String txt;
	public String node;
	public Date time;
	public DeltaMap procs;
	public DeltaMap ets;
	public DeltaMap mem;
	public DeltaMap stats;
	public MonitorEntry parent;

	public MonitorEntry(MonitorEntry parent, String str) {
		this.parent = parent;
		txt = new String(str);
		OtpErlangObject data = eval(txt);
		Bindings b;
		try {
			b = ErlUtils.match(
					"{erlide_monitor, N, [{time, T}, {processes, P}, "
							+ "{ets, E}, {memory, M}, {stats, S}]}", data);
			if (b != null) {
				node = b.getAtom("N");
				time = parseTime(b.get("T"));
				procs = parseDelta("Processes", b.getList("P"));
				ets = parseDelta("ETS", b.getList("E"));
				mem = parseDelta("Memory", b.getList("M"));
				stats = parseDelta("Statistics", b.getList("S"));
			}
		} catch (ParserException e) {
			e.printStackTrace();
		} catch (OtpErlangException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}
	}

	private DeltaMap parseDelta(String label, OtpErlangObject[] list) {
		DeltaMap result = new DeltaMap();
		result.label = label;
		for (OtpErlangObject item : list) {
			OtpErlangTuple t = (OtpErlangTuple) item;
			if (t.elementAt(0).toString().equals("added")) {
				result.added.addAll(parseMap(((OtpErlangList) t.elementAt(1))
						.elements()));
			}
			if (t.elementAt(0).toString().equals("deleted")) {
				result.deleted.addAll(parseMap(((OtpErlangList) t.elementAt(1))
						.elements()));
			}
			if (t.elementAt(0).toString().equals("modified")) {
				result.modified
						.addAll(parseMap(((OtpErlangList) t.elementAt(1))
								.elements()));
			}
		}
		return result;
	}

	private List<Map<String, OtpErlangObject>> parseMap(
			OtpErlangObject[] elements) {
		List<Map<String, OtpErlangObject>> result = new ArrayList<Map<String, OtpErlangObject>>();
		// TODO fill here
		for (OtpErlangObject item : elements) {
			// FIXME this doesn't work for 'memory' and 'stats'
			result.add(parseOneMap((OtpErlangList) item));
		}
		return result;
	}

	private Map<String, OtpErlangObject> parseOneMap(OtpErlangList item) {
		Map<String, OtpErlangObject> result = new HashMap<String, OtpErlangObject>();
		for (OtpErlangObject elem : item.elements()) {
			OtpErlangTuple tuple = (OtpErlangTuple) elem;
			String key = tuple.elementAt(0).toString();
			OtpErlangObject value = tuple.elementAt(1);
			result.put(key, value);
		}
		return result;
	}

	private Date parseTime(OtpErlangObject src) throws ParseException {
		DateFormat ef = new SimpleDateFormat("{{y,M,d},{H,m,s}}");
		return ef.parse(src.toString());
	}

	private OtpErlangObject eval(String str) {
		Backend b = ErlangCore.getBackendManager().getIdeBackend();
		final BackendEvalResult r = ErlBackend.eval(b, str, null);
		if (r.isOk()) {
			return r.getValue();
		}
		return null;
	}

	@Override
	public String toString() {
		if (node == null) {
			return ">>> " + txt;
		}
		StringBuilder s = new StringBuilder();
		s.append(new SimpleDateFormat().format(time)).append(" = ");
		s.append(procs.toString());
		return s.toString();
	}
}