const path              = require('path');
const webpack           = require('webpack');
const merge             = require('webpack-merge');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

console.log('Start Webpack process...');
const prod = 'production';
const dev = 'development';

// Determine build env by npm command options
const TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? prod : dev;
const isDev = TARGET_ENV == dev;
const isProd = TARGET_ENV == prod;
const ENV = {
  'googleMapApiKey':
    process.env.GOOGLE_MAP_API_KEY ||
      // This API key is only available on `stage.kucipong.com`.
      'AIzaSyA6yBv38YNHWzaAI5S7c27JfqkSFMFC-7g',
};

const outputPath = path.join(__dirname, 'dist');

// Common webpack config
const commonConfig = {
  output: {
    path: outputPath,
    filename: 'static/[name].js',
    publicPath: "/",
  },

  entry: {
    chat: [
      'webpack-dev-server/client?http://localhost:8080',
      path.join( __dirname, 'src/chat.js' )
    ],
    endUser: [
      'webpack-dev-server/client?http://localhost:8080',
      path.join( __dirname, 'src/endUser.js' )
    ],
    storeUser: [
      'webpack-dev-server/client?http://localhost:8080',
      path.join( __dirname, 'src/storeUser.js' )
    ],
    adminUser: [
      'webpack-dev-server/client?http://localhost:8080',
      path.join( __dirname, 'src/adminUser.js' )
    ],
  },

  resolve: {
    extensions: ['.js', '.elm'],
    modules: [
      'node_modules',
      path.resolve('src/elm'),
    ],
  },

  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.(eot|ttf|woff|woff2|svg)$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              name: "static/[name].[ext]",
            },
          },
        ]
      },
      {
        test: /\.pug$/,
        use: 'pug-loader',
      },
      {
        test: /\.(jpg|jpeg|png)$/,
        use: 'url-loader'
      },
    ]
  },

  plugins: [
      // Chat page
      {
        chunks: ['chat'],
        template: 'chat',
      },
    ].concat(
      [
        // Pages for end users.
        '404',
        'category',
        'coupon_id',
        'store_id',
        'store_id_coupon',
      ].map((template) => ({
        chunks: ['endUser'],
        template,
      }))
    ).concat(
      // Pages for store users.
      [
        'login',
        'store',
        'store_edit',
        'coupon',
        'coupon_delete',
        'coupon_id',
        'coupon_id_edit',
      ].map((template) => ({
        chunks: ['storeUser'],
        template,
        directory: 'src/pug/store',
        outputDir: 'store',
      }))
    ).concat(
      // Pages for admin users
      [
        'login',
        'store_create',
        'store_delete',
        'store_delete_confirm',
        'store_login',
      ].map((template) => ({
        chunks: ['adminUser'],
        template,
        directory: 'src/pug/admin',
        outputDir: 'admin',
      }))
    ).map((o) =>
      new HtmlWebpackPlugin({
        chunks: o.chunks,
        template: `${o.directory || 'src/pug'}/${o.template}.pug`,
        inject:   'body',
        filename: `templates/${o.outputDir || '.'}/${o.template}.html`,
        data: ENV,
        hash: true,
      })
    ).concat(
      [
        // Inject variables to JS file.
        new webpack.DefinePlugin({
          'process.env':
            Object.keys(ENV).reduce((o, k) =>
              merge(o, {
                [k]: JSON.stringify(ENV[k]),
              }), {}
            ),
        }),
      ]
    ),
};

// Settings for `npm start`
if (isDev) {
  console.log('Serving locally...');
  module.exports = merge(commonConfig, {

    devtool: 'source-map',
    devServer: {
      historyApiFallback: true,
      contentBase: 'src',
      inline: true,
      port: ENV.port,
      host: ENV.host,
    },

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: 'elm-hot-loader',
            },
            { loader: 'elm-webpack-loader',
              options: {
                verbose: true,
                warn: true,
                debug: true,
              },
            },
          ],
        },
        {
          test: /\.(css|scss)$/,
          use: [
            'style-loader',
            'css-loader',
            'resolve-url-loader',
            'sass-loader',
            'postcss-loader',
          ],
        },
      ],
    },
  });
}

// Settings for `npm run build`.
if (isProd) {
  console.log('Building for prod...');

  module.exports = merge(commonConfig, {

    module: {
      rules: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use:  'elm-webpack-loader',
        },
        {
          test: /\.(css|scss)$/,
          use: ExtractTextPlugin.extract({
            fallback: 'style-loader',
            use: [
              'css-loader',
              'resolve-url-loader',
              'sass-loader',
              'postcss-loader',
            ],
          }),
        },
      ],
    },

    plugins: [
      new CopyWebpackPlugin([
        // {
        //   from: 'src/favicon.ico'
        // },
      ]),

      // Extract CSS into a separate file
      new ExtractTextPlugin({
        filename: 'static/[name].css',
        disable: false,
        allChunks: true,
      }),

      // Minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
          minimize:   true,
          compressor: { warnings: false }
      }),
    ],
  });
}
