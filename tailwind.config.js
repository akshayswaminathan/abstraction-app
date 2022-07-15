tailwind.config = {
  theme: {
	// Add extra colors (replacing the defaults)
	  borderRadius: {
		  DEFAULT: '10px',
		  full: '9999px'
	  },
	  fontSize: {
		  sidebar: ['18px', '22px'],
		  monogram: ['16px', '19px'],
		  mini: ['12px', '10px'],
		  sidebarHeader: ['20px', '24px'],
		  bodyTitle: ['24px', '29px']
	  },
	  colors: {
		  grey: {
			  100: '#F6F7F9',
			  200: '#E6E8EF'
		  },
		  primary: '#153BAC',
		  muted: '#73798C',
		  white: '#FFFFFF',
		  black: '#000000',
		  transparent: 'transparent'
	  },
  },
}