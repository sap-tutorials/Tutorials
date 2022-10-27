//
//  SAPURLSession+Extension.swift
//  FioriARSceneExample
//
//  Created by Muessig, Kevin on 18.01.22.
//

import Foundation
import SAPFoundation

extension SAPURLSession {
    static func createOAuthURLSession(clientID: String, authURL: String, redirectURL: String, tokenURL: String) -> SAPURLSession {
        let session = SAPURLSession()
        session.attachOAuthObserver(clientID: clientID, authURL: authURL, redirectURL: redirectURL, tokenURL: tokenURL)
        return session
    }

    func attachOAuthObserver(clientID: String, authURL: String, redirectURL: String, tokenURL: String) {
        let secureKeyValueStore = SecureKeyValueStore()
        try! secureKeyValueStore.open(with: "downloadAR_secure_store")
        let compositeStore = CompositeStorage()
        try! compositeStore.setPersistentStore(secureKeyValueStore)

        if let authorizationEndpointURL = URL(string: authURL),
           let redirectURL = URL(string: redirectURL),
           let tokenEndpointURL = URL(string: tokenURL)
        {
            let params = OAuth2AuthenticationParameters(authorizationEndpointURL: authorizationEndpointURL,
                                                        clientID: clientID,
                                                        redirectURL: redirectURL,
                                                        tokenEndpointURL: tokenEndpointURL)

            let authenticator = OAuth2Authenticator(authenticationParameters: params, webViewPresenter: WKWebViewPresenter())
            let oauthObserver = OAuth2Observer(authenticator: authenticator, tokenStore: OAuth2TokenStorage(store: compositeStore))

            self.register(oauthObserver)
        }
    }
}
