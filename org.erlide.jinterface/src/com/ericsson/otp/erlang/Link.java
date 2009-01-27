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
class Link {
	private OtpErlangPid local;
	private OtpErlangPid remote;

	public Link(OtpErlangPid local, OtpErlangPid remote) {
		this.local = local;
		this.remote = remote;
	}

	public OtpErlangPid local() {
		return local;
	}

	public OtpErlangPid remote() {
		return remote;
	}

	public boolean contains(OtpErlangPid pid) {
		return (this.local.equals(pid) || this.remote.equals(pid));
	}

	public boolean equals(OtpErlangPid local, OtpErlangPid remote) {
		return ((this.local.equals(local) && this.remote.equals(remote)) || (this.local
				.equals(remote) && this.remote.equals(local)));
	}
}
