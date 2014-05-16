package org.erlide.ui.editors.erl.correction.fixes;

import java.io.IOException;
import java.io.InputStream;

final class EmptyInputStream extends InputStream {
    @Override
    public int read() throws IOException {
        return -1;
    }

    @Override
    public int available() throws IOException {
        return 0;
    }
}
