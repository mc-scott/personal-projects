import streamlit as st
import pandas as pd
import numpy as np
import re
from nltk import Text
from utils import fn_text_clean, fn_get_pdf_text, fn_highlight_selected_text, fn_downloads, fn_convert_df
from nltk.tokenize import word_tokenize, sent_tokenize

# in terminal of working directory:
# python -m streamlit run app.py
# this will open a network url where edits can be seen in real time on save and refresh

# download corpus' used by app and cashe
fn_downloads()

# title
st.header("PDF Text Explorer Tool")

uploaded_file = st.file_uploader("Choose a PDF file to explore", type="pdf",
                                 help= "Upload a PDF document from your 'pdf-docs' directory.")

if uploaded_file is not None:

    # read file as string
    data_load_state = st.text("Loading data...")
    try:
        text = fn_get_pdf_text(f"pdf-docs/{uploaded_file.name}")
    except Exception as e:
        st.error(f"""
                 {e}. Upload a file from your 'pdf-docs' sub-directory.
                 If this error persists, try uploading a different PDF. File
                 size is limited to 200MB. 
                 """)
        st.stop()
    data_load_state.text("Loaded Successfully")

    st.divider()

    st.subheader("Document Statistics:")
    st.markdown("""
                Some basic statistics about the document are provided below. 
                Some light text pre-processing has already been applied like converting to lower case 
                and removing non-ascii characters. If any email addresses are present in the document, 
                they will show in a second table.
                """)

    # initial cleaning of text:
    #   convert to lower, remove line breaks, normalise whitespace,
    #   remove non-ascii characters, remove hyperlinks
    #   remove leading/trailing whitespace
    text_light_cleaning = fn_text_clean(text)

    # get number of words and sentences in document
    num_words = len(word_tokenize(text_light_cleaning))
    num_sentences = len(sent_tokenize(text_light_cleaning))
    # create list of email addresses
    email_list = re.findall(r'[\w.+-]+@[\w-]+\.[\w.-]+', text_light_cleaning)

    # create summary table
    length_df = pd.DataFrame({
        "Number of words": [num_words],
        "Number of sentences": [num_sentences],
        "Email addresses found": [len(email_list)]
    }).T.rename(columns={0 : "Count"})

    # write summary table to app
    st.dataframe(length_df)

    # create email summary table if any are found
    if len(email_list) > 0:
        email_df = pd.DataFrame({
            "Email addresses": email_list
        })
        # display df
        st.dataframe(email_df, hide_index=True)

    st.divider()

    # bespoke text cleaning options
    st.subheader("Choose how to process text:")

    st.markdown("""
                Toggle the options below to change how the text is processed from this point on. 
                Changes will show in the document search section and word frequency section.
                """)
    col1, col2, col3 = st.columns(3)
    with col1:
        rm_punc = st.toggle("Remove punctuation", value= True)
        rm_stopwords = st.toggle("Remove stop words", value= False)
    with col2:
        rm_digits = st.toggle("Remove digits", value= False)
    with col3:
        rm_dates = st.toggle("Remove dates", value= False)

    st.text("Further processing options:")
    processing_type = st.radio(
        "Options",
        ["None", "Porter Stemmer", "Word Net Lemmatizer"],
        captions=[
            "Do not apply additional processing",
            "Reduce inflected words to common root",
            "Reduce word variants to their base form"
        ],
        horizontal= True,
        label_visibility= "collapsed")

    if processing_type == "None":
        method = None
    elif processing_type == "Porter Stemmer":
        method = "S"
    elif processing_type == "Word Net Lemmatizer":
        method = "L"

    text_cleaned = fn_text_clean(text,
                                 method=method,
                                 rm_punc=rm_punc,
                                 rm_digits=rm_digits,
                                 rm_dates=rm_dates,
                                 rm_stopwords=rm_stopwords)

    st.divider()

    st.subheader("Document search - concordance table")

    st.markdown("""
                Input a word or term and press enter to see matches reflected in the table below. 
                For example, `'Contribution'`, `'pensions regulator'`, `'scheme'`...
                """)

    col4, col5, col6 = st.columns(3)
    with col4:
        num_matches_to_show = st.slider("Matches to show", 1, 500, 15, step=5)

    # create input for concordance display
    term = st.text_input("Search word/term",
                         placeholder= "e.g. risk, Scheme, Defined Benefit",
                         help= """
                         Input a word or term to see matches populate a table with concordance lines
                         """).lower()

    term_list = term.split()

    st.text(f"Showing the top {num_matches_to_show} results...")

    # tokenise corpus by word
    text_to_analyse = word_tokenize(text_cleaned)

    if len(term_list) > 0:

        # instantiate a Text object
        text_to_analyse_obj = Text(text_to_analyse)

        con_list = text_to_analyse_obj.concordance_list(term_list, lines=num_matches_to_show)

        # create and display the concordance df
        concordance_df = pd.DataFrame({
            "Matching string": ["..." + con_list[i].line + "..." for i in range(len(con_list))]
        })
        # create a column to add html to table
        concordance_df["Matching text"] = concordance_df.apply(
            fn_highlight_selected_text,
            origin_col= "Matching string",
            term= term,
            axis=1)
        # display as markdown if values persent
        st.text(f"Matches to term: {len(concordance_df)}")

        concordance_df.index = np.arange(1, len(concordance_df)+1)

        st.markdown(concordance_df[["Matching text"]].to_html(escape= False),
                    unsafe_allow_html= True)

    st.divider()

    st.subheader("Single-word frequency summary table")

    st.markdown("""
                Enter words into the box below to see their count reflected in a table.
                Download the table as a .csv file.
                """)

    words_df = pd.DataFrame({
            "all_words": list(set(text_to_analyse))
        })

    terms_to_summarise = st.multiselect(
        "Edit word summary table",
        options= words_df,
        help= "Choose multiple terms you would like see summarised below",
        placeholder= "e.g. risk, scheme, liability, regulator")
    
    terms_summary_df = pd.DataFrame()

    if len(terms_to_summarise) > 0:

        for term in terms_to_summarise:
            term_num = len(re.findall(str(term.lower()), text_cleaned))

            terms_summary_df[term] = [term_num]

        display_summary_df = terms_summary_df.T.rename(columns={0 : "Count"})
        display_summary_df.index.name = "Term"

        st.dataframe(display_summary_df, use_container_width=True)

        csv = fn_convert_df(display_summary_df)

        st.download_button(
            label="Download data as CSV",
            data=csv,
            file_name=f"{uploaded_file.name[:20]}..._summary.csv",
            mime="text/csv")

#TODO Loading in a file from any location on machine
#TODO word n-grams visual with interaction widgets
