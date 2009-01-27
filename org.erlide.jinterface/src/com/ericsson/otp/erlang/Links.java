/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
package com.ericsson.otp.erlang;

// package scope
class Links {
	Link[] links;
	int count;

	Links() {
		this(10);
	}

	Links(int initialSize) {
		links = new Link[initialSize];
		count = 0;
	}

	synchronized void addLink(OtpErlangPid local, OtpErlangPid remote) {
		int i;

		if ((i = find(local, remote)) == -1) {
			if (count >= links.length) {
				Link[] tmp = new Link[count * 2];
				System.arraycopy(links, 0, tmp, 0, count);
				links = tmp;
			}
			links[count++] = new Link(local, remote);
		}
	}

	synchronized void removeLink(OtpErlangPid local, OtpErlangPid remote) {
		int i;

		if ((i = find(local, remote)) != -1) {
			count--;
			links[i] = links[count];
			links[count] = null;
		}
	}

	synchronized boolean exists(OtpErlangPid local, OtpErlangPid remote) {
		return (find(local, remote) != -1);
	}

	synchronized int find(OtpErlangPid local, OtpErlangPid remote) {
		for (int i = 0; i < count; i++) {
			if (links[i].equals(local, remote))
				return i;
		}
		return -1;
	}

	int count() {
		return count;
	}

	/* all local pids get notified about broken connection */
	synchronized OtpErlangPid[] localPids() {
		OtpErlangPid[] ret = null;
		if (count != 0) {
			ret = new OtpErlangPid[count];
			for (int i = 0; i < count; i++) {
				ret[i] = links[i].local();
			}
		}
		return ret;
	}

	/* all remote pids get notified about failed pid */
	synchronized OtpErlangPid[] remotePids() {
		OtpErlangPid[] ret = null;
		if (count != 0) {
			ret = new OtpErlangPid[count];
			for (int i = 0; i < count; i++) {
				ret[i] = links[i].remote();
			}
		}
		return ret;
	}

	/* clears the link table, returns a copy */
	synchronized Link[] clearLinks() {
		Link[] ret = null;
		if (count != 0) {
			ret = new Link[count];
			for (int i = 0; i < count; i++) {
				ret[i] = links[i];
				links[i] = null;
			}
			count = 0;
		}
		return ret;
	}

	/* returns a copy of the link table */
	synchronized Link[] links() {
		Link[] ret = null;
		if (count != 0) {
			ret = new Link[count];
			System.arraycopy(links, 0, ret, 0, count);
		}
		return ret;
	}
}
