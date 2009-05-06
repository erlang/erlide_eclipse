package erlang;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FunctionRef {
	public final String module;
	public final String function;
	public final int arity;

	public FunctionRef(final String module, final String function,
			final int arity) {
		this.module = module;
		this.function = function;
		this.arity = arity;
	}

	@SuppressWarnings("boxing")
	public FunctionRef(final OtpErlangObject e) {
		if (e instanceof OtpErlangTuple) {
			final OtpErlangTuple t = (OtpErlangTuple) e;
			try {
				final Bindings bb = ErlUtils.match("{M:a, F:a, A:i}", t);
				final String m = bb.getAs("M", String.class);
				final String f = bb.getAs("F", String.class);
				final int a = bb.getAs("A", Integer.class);

				this.module = m;
				this.function = f;
				this.arity = a;
			} catch (final Exception e2) {
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
