/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* List of projects/orgs using your project for the users page */
const users = [
  {
    caption: 'BNOSAC',
    image: '/udpipe/img/logo-bnosac.png',
    infoLink: 'http://www.bnosac.be',
    pinned: true,
  },
  {
    caption: 'txtminer',
    image: '/udpipe/img/logo-txtminer.png',
    pinned: true,
  },
  {
    caption: 'cleanNLP',
    infoLink: 'https://cran.r-project.org/web/packages/cleanNLP/index.html',
    image: '/udpipe/img/logo-cleannlp.png',
    pinned: true,
  }
];

const siteConfig = {
  title: 'NLP with R and UDPipe',
  tagline: 'Tokenization, Parts of Speech Tagging, Lemmatization, Dependency Parsing and NLP flows',
  url: 'https://bnosac.github.io' /* your website url */,
  baseUrl: '/udpipe/' /* base url for your project */,
  projectName: 'udpipe',
  disableHeaderTitle : true,
  headerLinks: [
    {href: 'https://bnosac.github.io/udpipe/en/index.html', label: 'Home' },
    {doc: 'doc0', label: 'Docs'},
    {page: 'help', label: 'Support'},
    {href: 'https://github.com/bnosac/udpipe', label: 'GitHub' },
    {blog: true, label: 'Blog'},
    { search: true },
  // Determines language drop down position among links
  { languages: true }
  ],
  users,
  /* path to images for header/footer */
  headerIcon: 'img/logo-udpipe-r.png',
  footerIcon: 'img/logo-udpipe-r.png',
  favicon: 'img/favicon.ico',
  /* colors for website */
  colors: {
    primaryColor: '#2E8555',
    secondaryColor: '#205C3B',
  },
  // This copyright info is used in /core/Footer.js and blog rss/atom feeds.
  copyright: 'Copyright © ' + new Date().getFullYear() + ' BNOSAC',
  organizationName: 'bnosac', // or set an env variable ORGANIZATION_NAME
  projectName: 'udpipe', // or set an env variable PROJECT_NAME
  
  highlight: {
    // Highlight.js theme to use for syntax highlighting in code blocks
    theme: 'default',
  },
  scripts: ['https://buttons.github.io/buttons.js'],
  // You may provide arbitrary config keys to be used as needed by your template.
  repoUrl: 'https://github.com/bnosac/udpipe',
};

module.exports = siteConfig;
