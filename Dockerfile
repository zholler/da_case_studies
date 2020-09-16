# base image from modified nbgallery jupyter-alpine: ubuntu based jupyter docker
FROM jbiro93/da_cases_py:latest

# copy project folders into container
COPY ./da_case_studies/ ./da_case_studies/
RUN mkdir ./da_case_studies2
COPY ./da_data_repo/ ./da_data_repo/

# install requirements
RUN pip3 install cython tornado asyncio notebook
RUN pip3 install -r ./da_case_studies/ch00-tech-prep/requirements.txt
RUN pip3 install git+https://github.com/janosbiro/statsmodels
RUN pip3 install --upgrade notebook
