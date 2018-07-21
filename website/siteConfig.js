const siteConfig = {
  title: 'reason-dre',
  tagline: 'Declarative bindings for Reason',
  url: 'https://rrdelaney.github.io',
  baseUrl: '/reason-dre/',

  projectName: 'reason-dre',
  organizationName: 'rrdelaney',

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'writing-declarations', label: 'Docs'},
    {blog: true, label: 'Blog'},
  ],

  /* path to images for header/footer */
  headerIcon: 'img/dre_logo.png',
  footerIcon: 'img/dre_logo.png',
  favicon: 'img/favicon.png',

  /* colors for website */
  colors: {
    primaryColor: '#ffffff',
    secondaryColor: '#db4d3f',
  },

  /* custom fonts for website */
  /*fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },*/

  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
  },

  // Add custom scripts here that would be placed in <script> tags
  scripts: ['https://buttons.github.io/buttons.js'],

  /* On page navigation for the current documentation page */
  onPageNav: 'separate',

  /* Open Graph and Twitter card images */
  ogImage: 'img/dre_logo.png',
  twitterImage: 'img/dre_logo.png',
};

module.exports = siteConfig;
