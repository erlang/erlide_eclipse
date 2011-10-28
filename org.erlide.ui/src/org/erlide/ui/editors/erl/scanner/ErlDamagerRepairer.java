/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.scanner;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.Token;
import org.erlide.jinterface.ErlLogger;

public class ErlDamagerRepairer extends DefaultDamagerRepairer {

    public ErlDamagerRepairer(final ITokenScanner scanner) {
        super(scanner);
    }

    /*
     * @see IPresentationRepairer#createPresentation(TextPresentation,
     * ITypedRegion)
     */
    @Override
    public void createPresentation(final TextPresentation presentation,
            final ITypedRegion region) {

        if (fScanner == null) {
            // will be removed if deprecated constructor will be removed
            addRange(presentation, region.getOffset(), region.getLength(),
                    fDefaultTextAttribute);
            return;
        }

        int lastStart = region.getOffset();
        int length = 0;
        boolean firstToken = true;
        IToken lastToken = Token.UNDEFINED;
        TextAttribute lastAttribute = getTokenTextAttribute(lastToken);

        fScanner.setRange(fDocument, lastStart, region.getLength());

        while (true) {
            final IToken token = fScanner.nextToken();
            if (token == null) {
                try {
                    ErlLogger.warn("null token from '%s'>>>",
                            fDocument.get(lastStart, region.getLength()));
                } catch (final BadLocationException e) {
                    ErlLogger.warn("null token from '%s'", fDocument.get());
                }
                break;
            }
            if (token.isEOF()) {
                break;
            }

            final TextAttribute attribute = getTokenTextAttribute(token);
            // if (lastAttribute != null && lastAttribute.equals(attribute)) {
            // length += fScanner.getTokenLength();
            // firstToken= false;
            // } else {
            if (!firstToken) {
                addRange(presentation, lastStart, length, lastAttribute);
            }
            firstToken = false;
            lastToken = token;
            lastAttribute = attribute;
            lastStart = fScanner.getTokenOffset();
            length = fScanner.getTokenLength();
            // }
        }

        addRange(presentation, lastStart, length, lastAttribute);
    }

}
