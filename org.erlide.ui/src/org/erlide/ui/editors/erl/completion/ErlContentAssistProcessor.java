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

import java.util.EnumSet;

import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.services.IDisposable;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.prefs.plugin.CodeAssistPreferences;

import com.ericsson.otp.erlang.OtpErlangAtom;

public class ErlContentAssistProcessor extends AbstractErlContentAssistProcessor
        implements IDisposable {

    protected char[] fCompletionProposalAutoActivationCharacters;
    protected final IPreferenceChangeListener fPreferenceChangeListener;

    public ErlContentAssistProcessor(final ISourceViewer sourceViewer,
            final IErlModule module, final IErlProject project,
            final ContentAssistant contentAssistant) {
        super(sourceViewer, module, project, contentAssistant);
        fPreferenceChangeListener = new PreferenceChangeListener();
        final IEclipsePreferences node = CodeAssistPreferences.getNode();
        node.addPreferenceChangeListener(fPreferenceChangeListener);
    }

    @Override
    public char[] getCompletionProposalAutoActivationCharacters() {
        return fCompletionProposalAutoActivationCharacters;
    }

    @Override
    protected EnumSet<Kinds> filterFlags(final EnumSet<Kinds> flags) {
        return flags;
    }

    @Override
    protected String quoted(final String string, final Kinds kind) {
        if (kind == Kinds.INCLUDES || kind == Kinds.INCLUDE_LIBS) {
            return "\"" + string + "\"";
        }
        if (kind == Kinds.MODULES) {
            final String string1 = string.substring(0, string.length() - 1);
            return new OtpErlangAtom(string1).toString() + ":";
        }
        return string;
    }

    public void setToPrefs() {
        final CodeAssistPreferences prefs = new CodeAssistPreferences();
        prefs.load();
        fCompletionProposalAutoActivationCharacters = prefs.getErlangTriggers()
                .toCharArray();
        contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
        contentAssistant.enableAutoActivation(prefs.isAutoActivate());
        contentAssistant.setAutoActivationDelay(prefs.getDelayInMS());
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
