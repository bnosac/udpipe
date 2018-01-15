/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require('react');

const CompLibrary = require('../../core/CompLibrary.js');
const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

const siteConfig = require(process.cwd() + '/siteConfig.js');

function imgUrl(img) {
  return siteConfig.baseUrl + 'img/' + img;
}

function docUrl(doc, language) {
  return siteConfig.baseUrl + 'docs/' + (language ? language + '/' : '') + doc;
}

function pageUrl(page, language) {
  return siteConfig.baseUrl + (language ? language + '/' : '') + page;
}

class Button extends React.Component {
  render() {
    return (
      <div className="pluginWrapper buttonWrapper">
        <a className="button" href={this.props.href} target={this.props.target}>
          {this.props.children}
        </a>
      </div>
    );
  }
}

Button.defaultProps = {
  target: '_self',
};

const SplashContainer = props => (
  <div className="homeContainer">
    <div className="homeSplashFade">
      <div className="wrapper homeWrapper">{props.children}</div>
    </div>
  </div>
);

const Logo = props => (
  <div className="projectLogo">
    <img src={props.img_src} />
  </div>
);

const ProjectTitle = props => (
  <h2 className="projectTitle">
    {siteConfig.title}
    <small>{siteConfig.tagline}</small>
  </h2>
);

const PromoSection = props => (
  <div className="section promoSection">
    <div className="promoRow">
      <div className="pluginRowBlock">{props.children}</div>
    </div>
  </div>
);

class HomeSplash extends React.Component {
  render() {
    let language = this.props.language || '';
    return (
      <SplashContainer>
        <Logo img_src={imgUrl('logo-udpipe-r.png')} />
        <div className="inner">
          <ProjectTitle />
          <PromoSection>
            <Button href={docUrl('doc0.html', language)}>Try it out</Button>
            <Button href={docUrl('doc1.html', language)}>Overview</Button>
            <Button href={docUrl('doc2.html', language)}>Detailed usage</Button>
            <Button href={docUrl('doc3.html', language)}>Model building</Button>
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

const Block = props => (
  <Container
    padding={['bottom', 'top']}
    id={props.id}
    background={props.background}>
    <GridBlock align="center" contents={props.children} layout={props.layout} />
  </Container>
);

const Features = props => (
  <Block layout="fourColumn">
    {[
      {
        content: 'Quick and simple annnotations giving rich output: tokenization, tagging, lemmatization and dependency parsing',
        image: imgUrl('gem.svg'),
        imageAlign: 'top',
        title: 'Easy annotation',
      },
      {
        content: 'Multi-language support. From raw text to parsed output for more than 50 languages. Load pre-trained models build on CONLL-U data from universaldependencies.org',
        image: imgUrl('language.svg'),
        imageAlign: 'top',
        title: 'Language agnostic',
      },
      {
        content: 'No dependency on Python or Java. Just install.packages("udpipe") and you are up and running. No heavy R package dependencies for easy installation and maintenance.',
        image: imgUrl('play.svg'),
        imageAlign: 'top',
        title: 'Easy R installation',
      },
      {
        content: 'Train your own models directly on the newest CONLL-U data and tune them to your needs using local or up-to-date CONLL-U data.',
        image: imgUrl('studiovinari.svg'),
        imageAlign: 'top',
        title: 'Train & tune your own models',
      },
    ]}
  </Block>
);


const Showcase = props => {
  if ((siteConfig.users || []).length === 0) {
    return null;
  }
  const showcase = siteConfig.users
    .filter(user => {
      return user.pinned;
    })
    .map((user, i) => {
      return (
        <a href={user.infoLink} key={i}>
          <img src={user.image} title={user.caption} />
        </a>
      );
    });

  return (
    <div className="productShowcaseSection paddingBottom">
      <h2>{"Who's Using This?"}</h2>
      <p>This project is used by all these people</p>
      <div className="logos">{showcase}</div>
      <div className="more-users">
        <a className="button" href={pageUrl('users.html', props.language)}>
          More {siteConfig.title} Users
        </a>
      </div>
    </div>
  );
};

class Index extends React.Component {
  render() {
    let language = this.props.language || '';

    return (
      <div>
        <HomeSplash language={language} />
        <div className="mainContainer">
          <Features />
          <Showcase language={language} />
        </div>
      </div>
    );
  }
}

module.exports = Index;
