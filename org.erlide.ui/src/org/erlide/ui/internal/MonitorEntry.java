/**
 * 
 */
package org.erlide.ui.internal;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendEvalResult;
import org.erlide.jinterface.backend.ErlBackend;
import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlUtils;
import org.erlide.jinterface.util.ParserException;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

class MonitorEntry {

	public String txt;
	public String node;
	public Date time;
	public OtpErlangObject[] procs;
	public OtpErlangObject[] ets;
	public OtpErlangObject[] mem;
	public OtpErlangObject[] stats;
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
				procs = b.getList("P");
				ets = b.getList("E");
				mem = b.getList("M");
				stats = b.getList("S");
			}
		} catch (ParserException e) {
			e.printStackTrace();
		} catch (OtpErlangException e) {
			e.printStackTrace();
		} catch (ParseException e) {
			e.printStackTrace();
		}
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
}