package erlang;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FunctionRef {
	public final String module;
	public final String function;
	public final int arity;

	public FunctionRef(String module, String function, int arity) {
		this.module = module;
		this.function = function;
		this.arity = arity;
	}

	@SuppressWarnings("boxing")
	public FunctionRef(OtpErlangObject e) {
		if (e instanceof OtpErlangTuple) {
			OtpErlangTuple t = (OtpErlangTuple) e;
			try {
				Bindings bb = ErlUtils.match("{M:a, F:a, A:i}", t);
				String m = bb.getAs("M", String.class);
				String f = bb.getAs("F", String.class);
				int a = bb.getAs("A", Integer.class);

				this.module = m;
				this.function = f;
				this.arity = a;
			} catch (Exception e2) {
				throw new IllegalArgumentException();
			}

		} else {
			throw new IllegalArgumentException();
		}
	}

	@Override
	public String toString() {
		return module + ":" + function + "/" + arity;
	}

}
