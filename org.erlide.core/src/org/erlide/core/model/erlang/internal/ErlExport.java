package org.erlide.core.model.erlang.internal;

import org.erlide.core.model.erlang.IErlExport;
import org.erlide.core.model.root.api.IParent;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlExport extends ErlImportExport implements IErlExport {

    private static final int LABEL_LENGTH_LIMIT = 100;
    private final String functions;

    public ErlExport(final IParent parent, final OtpErlangList functionList,
            final String functions) {
        super(parent, "export", functionList);
        this.functions = functions;
    }

    public Kind getKind() {
        return Kind.EXPORT;
    }

    @Override
    public String toString() {
        return getName() + ": " + functions;
    }

    @Override
    public String getLabelString() {
        String s = functions;
        if (s.length() > LABEL_LENGTH_LIMIT) {
            int i = s.indexOf(',', LABEL_LENGTH_LIMIT);
            if (i == -1) {
                i = LABEL_LENGTH_LIMIT;
            }
            s = s.substring(0, i) + "...";
        }
        return s;
    }

}
