/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

const siteConfig = require(process.cwd() + '/siteConfig.js');

class Help extends React.Component {
  render() {
    const supportLinks = [
      {
        content:
          'Learn more using the [documentation on this site.](/udpipe/docs/doc0.html)',
        title: 'Browse Docs',
      },
      {
        content: 'Ask questions about the documentation and project',
        title: 'Join the community',
      },

    ];

    return (
      <div className="docMainWrapper wrapper">
        <Container className="mainContainer documentContainer postContainer">
          <div className="post">
            <header className="postHeader">
              <h2>Support?</h2>
            </header>

             <ul>
              <li>This project is maintained by a BNOSAC.</li>
              <li>Need support in text mining? Contact us at <a href="http://www.bnosac.be">http://www.bnosac.be</a></li>
             </ul> 
        <a
          href="http://www.bnosac.be"
          target="_blank"
          className="fbOpenSource">
          <img
            src="http://www.bnosac.be/images/bnosac/logo-bnosac.png"
            alt="BNOSAC Open Source"
            width="170"
            height="45"
          />
        </a>
        
            <GridBlock contents={supportLinks} layout="twoColumn" />
          </div>
        </Container>
      </div>
    );
  }
}

module.exports = Help;
