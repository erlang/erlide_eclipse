package org.erlide.engine.services.parsing;

import java.util.List;

import com.google.common.collect.Lists;

public class NullScannerService implements SimpleScannerService {

    @Override
    public List<ErlToken> lightScanString(final String string, final int offset)
            throws ScannerException {
        final ErlToken token = new ErlToken(ErlToken.KIND_OTHER, offset,
                string.length());
        return Lists.newArrayList(token);
    }
}
