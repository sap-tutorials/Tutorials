//
//  UserDefaultsHandler.swift
//  FioriARSceneExample
//
//  Created by Muessig, Kevin on 26.01.22.
//

import Foundation

struct UserDefaultsHandler {
    static func encode(scenes: [ARScene]) -> Data? {
        let encoder = JSONEncoder()
        do {
            return try encoder.encode(scenes)
        } catch {
            print("things")
            return nil
        }
    }
    
    static func decodeSceneIDs() -> [ARScene]? {
        if let sceneIDs = UserDefaults.standard.data(forKey: ScenePersistence.key.rawValue) {
            do {
                let decoder = JSONDecoder()
                
                return try decoder.decode([ARScene].self, from: sceneIDs)
            } catch {
                print("")
            }
        }
        return nil
    }
    
}
