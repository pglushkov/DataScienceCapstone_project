library(shiny)
shinyUI(
	pageWithSidebar(
		headerPanel('Data Science Specialization, Capstone Project, July 2016. Student : Peter Glushkov.'),

		sidebarPanel(
			h3('INPUT'),
			textInput('input_text', 'Input Text', value = ""),
			submitButton('Submit'),

			h3('Description and short manual'),
			p('This simple application demonstates possibility of text-mining algorithms and technologies in their
			application to the task of Text Prediction. Application usses N-gram models (a simple mixture of 2-gram 
			and 3-gram models to be precise) that were built by processing materials, supplied for the Course Project.
			To get more detailed desciption of the application, you can refer to further resources:'),
			p('* short description of the app  : http://LINK_TO_SLIDES'),
			p('* repository with the code      : http://LINK_TO_THE_REPO'),
			p('Short manual:'),
			p('1) type a phrase into INPUT field'),
			p('2) push the SUBMIT button'),
			p('3) prediction will be written in the PREDICTION field shortly'),
			p('4) another likely candidates for prediction will be written in the POSSIBLE CANDIDATES field'),
			p('5) in case if predicion takes more then N seconds - something probably went broken, you should reload the
			page by pressing F5 button in your browser'),
			p('- have fun :-)')
		),

		mainPanel(
			h1('Results of prediction'),

			h3('PREDICTION'),
			verbatimTextOutput('main_prediction'),
			p('   '),
			p('   '),
			h3('POSSIBLE CANDIDATES'),
			verbatimTextOutput('likely_candidates'),
			p('*  '),
			p('*  '),
			p('*  '),
			p('*  '),
			p('*  '),
			p('*  '),
			p('*  '),
			p(' ----------------------------------------------------------------- '),
			h3('DIGANOSTICS'),
			verbatimTextOutput('DEBUG')
		)
	)
)
