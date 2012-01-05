/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl.completion;

import java.util.Set;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.services.IDisposable;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.prefs.plugin.CodeAssistPreferences;
import org.osgi.service.prefs.BackingStoreException;

public class ErlContentAssistProcessor extends
        AbstractErlContentAssistProcessor implements IContentAssistProcessor,
        IDisposable {

    protected char[] fCompletionProposalAutoActivationCharacters;
    protected final IPreferenceChangeListener fPreferenceChangeListener;

    public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final ContentAssistant contentAssistant) {
        super(sourceViewer, module, contentAssistant);
        fPreferenceChangeListener = new PreferenceChangeListener();
        final IEclipsePreferences node = CodeAssistPreferences.getNode();
        node.addPreferenceChangeListener(fPreferenceChangeListener);
    }

    @Override
    public IContextInformation[] computeContextInformation(
            final ITextViewer viewer, final int offset) {
        return null;
    }

    @Override
    public char[] getContextInformationAutoActivationCharacters() {
        return null;
    }

    @Override
    public String getErrorMessage() {
        return null;
    }

    @Override
    public IContextInformationValidator getContextInformationValidator() {
        return null;
    }

    @Override
    public char[] getCompletionProposalAutoActivationCharacters() {
        return fCompletionProposalAutoActivationCharacters;
    }

    @Override
    protected Set<Kinds> filterFlags(final Set<Kinds> flags) {
        return flags;
    }

    @Override
    protected String quoted(final String string, final Kinds kind) {
        if (kind == Kinds.INCLUDES || kind == Kinds.INCLUDE_LIBS) {
            return "\"" + string + "\"";
        } else {
            return string;
        }
    }

    public void setToPrefs() {
        final CodeAssistPreferences prefs = new CodeAssistPreferences();
        try {
            prefs.load();
            fCompletionProposalAutoActivationCharacters = prefs
                    .getErlangTriggers().toCharArray();
            contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
            contentAssistant.enableAutoActivation(prefs.isAutoActivate());
            contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
        } catch (final BackingStoreException e) {
            fCompletionProposalAutoActivationCharacters = new char[0];
        }
    }

    private class PreferenceChangeListener implements IPreferenceChangeListener {
        @Override
        public void preferenceChange(final PreferenceChangeEvent event) {
            setToPrefs();
        }
    }

    @Override
    public void dispose() {
        final IEclipsePreferences node = CodeAssistPreferences.getNode();
        node.removePreferenceChangeListener(fPreferenceChangeListener);
    }

}
