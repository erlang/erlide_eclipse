package org.erlide.ui.views.console;

import java.util.ArrayList;
import java.util.Collections;
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
		req.setTstamp(new IoRequest.Timestamp());
		insertSorted(req);
	}

	private void insertSorted(IoRequest req) {
		synchronized (requests) {
			int index = Collections.binarySearch(requests, req);
			if (index < 0) {
				int is = 0;
				if ((-index - 1) <= 0)
					is = 0;
				else {
					IoRequest r = requests.get(-index - 2);
					is = r.getStart() + r.getLength();
				}
				req.setStart(is);
				requests.add(-index - 1, req);
			} else {
				requests.add(index, req);
			}
		}
	}

	public int add(OtpErlangObject msg, int start) {
		if (!(msg instanceof OtpErlangTuple))
			return 0;
		IoRequest req = new IoRequest((OtpErlangTuple) msg);

		// TODO this is to filter out process list events
		if (req.getMessage() == null) {
			return 0;
		}

		// TODO use a configuration for this
		// TODO maybe we should count text lines?
		synchronized (requests) {
			if (requests.size() > 5000) {
				for (int i = 0; i < 1000; i++) {
					requests.remove(0);
				}
			}
		}
		insertSorted(req);
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
