package org.erlide.ui.views.console;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlConsoleDocument {

	private final List<IoRequest> requests;

	public ErlConsoleDocument() {
		requests = new ArrayList<IoRequest>(1000);
	}

	public List<IoRequest> getContent() {
		synchronized (requests) {
			return new ArrayList<IoRequest>(requests);
		}
	}

	public void input(String s) {
		IoRequest req = new IoRequest(s);
		synchronized (requests) {
			requests.add(req);
		}
	}

	public int add(OtpErlangObject msg, int start) {
		IoRequest req = new IoRequest((OtpErlangTuple) msg);
		req.setStart(start);

		// TODO this is to filter out process list events
		if (req.getMessage() == null) {
			return 0;
		}

		// TODO use a configuration for this
		synchronized (requests) {
			if (requests.size() > 5000) {
				for (int i = 0; i < 1000; i++) {
					requests.remove(0);
				}
			}
			requests.add(req);
		}
		return req.getLength();
	}

	public IoRequest findAtPos(int pos) {
		synchronized (requests) {
			for (Object element : requests) {
				IoRequest req = (IoRequest) element;
				if (req.getStart() <= pos
						&& req.getStart() + req.getLength() > pos) {
					return req;
				}
			}
			return null;
		}
	}

	public List<IoRequest> getAllFrom(OtpErlangPid sender) {
		List<IoRequest> result = new ArrayList<IoRequest>(10);
		for (Object element0 : requests) {
			IoRequest element = (IoRequest) element0;
			if (element.getSender().equals(sender)) {
				result.add(element);
			}
		}
		return result;
	}

	public void add(List<OtpErlangObject> msgs, int start) {
		int ofs = start;
		for (Object element0 : msgs) {
			OtpErlangObject element = (OtpErlangObject) element0;
			ofs += add(element, ofs);
		}

	}

}
