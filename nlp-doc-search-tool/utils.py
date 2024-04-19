# import libraries
import re
import nltk
from PyPDF2 import PdfReader
import streamlit as st

from nltk.stem import WordNetLemmatizer
from nltk.stem import PorterStemmer
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords

@st.cache_data
def fn_downloads():
    """
    Download corpus' used in app and cashe results
    """
    nltk.download("punkt")
    nltk.download('wordnet')
    nltk.download('stopwords')
# end

# functions:

@st.cache_data
def fn_get_pdf_text(filepath:str) -> str:
    """
    Extract text from PDF files
    """
    assert isinstance(filepath, str), "ERROR: filepath must be a string."

    output = ""
    pdf_reader = PdfReader(filepath)
    for page in pdf_reader.pages:
        output += page.extract_text()

    return output
# end

@st.cache_data
def fn_text_clean(text: str,
                  method: str = None,
                  rm_punc: bool = False,
                  rm_stopwords: bool = False,
                  rm_digits: bool = False,
                  rm_dates: bool = False) -> str:
    '''
    Cleans text by removing elements with no value. Optionally remove 
    punctuaiton, stop words, digits, dates. 
    Optionally lemmatize or stem words. 
    Method = 'S': Porter Stemmer; 'L': Word Net Lemmatizer
    '''

    assert method is None or method == "S" or method == "L", "ERROR: Method must be None, 'S', or 'L'."

    text = re.sub(r'\n',' ',text)   #remove line breaks
    text = text.lower() #convert to lowercase
    text = re.sub(r'[^\x00-\x7f]', r'', text)   #remove non-ascii
    text = re.sub(r'https?:\/\/.*[\r\n]*', '', text)   #remove hyperlinks
    text = re.sub(r'\s+', ' ', text) #normalise white space
    text = text.strip() #remove leading/trailing white spaces

    #remove punctuation
    if rm_punc:
        text = re.sub(r'[^\w\s]', '', text)

    #remove stop words
    if rm_stopwords:
        filtered_tokens = [word for word in word_tokenize(text) if not word in set(stopwords.words('english'))]
        text = " ".join(filtered_tokens)

    #additional pre-processing: remove digits/currencies
    if rm_digits:
        text = re.sub(r"\d+","",text)
        text = re.sub(r'[\$\d+\d+\$]', "", text)

    #remove dates
    if rm_dates:
        text = re.sub(r'\d+[\.\/-]\d+[\.\/-]\d+', '', text)

    #lemmatisation: typically preferred over stemming
    if method == 'L':
        lemmer = WordNetLemmatizer()
        lemm_tokens = [lemmer.lemmatize(word) for word in word_tokenize(text)]
        text = " ".join(lemm_tokens)
        return text

    #stemming
    if method == 'S':
        porter = PorterStemmer()
        stem_tokens = [porter.stem(word) for word in word_tokenize(text)]
        text = " ".join(stem_tokens)
        return text

    return text
# end

@st.cache_data
def fn_highlight_selected_text(row, origin_col: str, term: str):
    '''
    Rowwise evaluation of df to find and replace a search term
    with html that highlight the given term
    '''
    text = row[origin_col]
    text = text.replace(term, f"<strong style='color: #E61E62'>{term}</strong>")
    return text
# end

@st.cache_data
def fn_convert_df(df):
    """
    Convert a dataframe to csv file and utf-8 encode
    """
    return df.to_csv().encode("utf-8")
# end
